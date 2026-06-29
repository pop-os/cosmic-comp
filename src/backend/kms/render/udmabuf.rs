use std::{
    io,
    os::fd::{AsFd, AsRawFd, FromRawFd, OwnedFd, RawFd},
    sync::LazyLock,
};

use rustix::{
    fs::{MemfdFlags, Mode, OFlags, SealFlags, fcntl_add_seals, ftruncate, memfd_create},
    ioctl::{Ioctl, Opcode, ioctl, opcode},
    param::page_size,
};
use smithay::{
    backend::allocator::{
        Allocator, Fourcc, Modifier,
        dmabuf::{Dmabuf, DmabufFlags},
        format::get_bpp,
    },
    utils::Size,
};

static PAGE_SIZE: LazyLock<usize> = LazyLock::new(page_size);

pub struct UdmabufAllocator {
    dev: OwnedFd,
}

pub const STRIDE_ALIGN: usize = 256;

impl UdmabufAllocator {
    pub fn new() -> io::Result<UdmabufAllocator> {
        let fd = rustix::fs::open(
            "/dev/udmabuf",
            OFlags::RDONLY | OFlags::CLOEXEC,
            Mode::empty(),
        )?;
        Ok(UdmabufAllocator { dev: fd })
    }
}

impl Allocator for UdmabufAllocator {
    type Buffer = Dmabuf;
    type Error = io::Error;

    fn create_buffer(
        &mut self,
        width: u32,
        height: u32,
        fourcc: Fourcc,
        modifiers: &[Modifier],
    ) -> Result<Self::Buffer, Self::Error> {
        let width = width as usize;
        let height = height as usize;
        if width >= u16::MAX as usize || height >= u16::MAX as usize {
            return Err(io::ErrorKind::Unsupported.into());
        }
        if !modifiers.contains(&Modifier::Linear) {
            return Err(io::ErrorKind::Unsupported.into());
        }
        let stride = (width * get_bpp(fourcc).ok_or(io::ErrorKind::Unsupported)?)
            .next_multiple_of(STRIDE_ALIGN);
        let page_mask = !(*PAGE_SIZE - 1);
        let size = (height * stride + (*PAGE_SIZE - 1)) & page_mask;

        let mem_fd = memfd_create("udmabuf", MemfdFlags::ALLOW_SEALING | MemfdFlags::CLOEXEC)?;
        ftruncate(&mem_fd, size as u64)?;
        fcntl_add_seals(&mem_fd, SealFlags::SHRINK)?;

        let dma_fd = udmabuf_from_memfd(&self.dev, mem_fd.as_fd(), 0, size as u64)?;
        let mut dmabuf = Dmabuf::builder(
            Size::new(width as i32, height as i32),
            fourcc,
            Modifier::Linear,
            DmabufFlags::empty(),
        );
        dmabuf.add_plane(dma_fd, 0, 0, stride as u32);
        dmabuf
            .build()
            .ok_or_else(|| io::ErrorKind::Unsupported.into())
    }
}

fn udmabuf_from_memfd(
    fd: impl AsFd,
    mem_fd: impl AsRawFd,
    offset: u64,
    size: u64,
) -> io::Result<OwnedFd> {
    #[repr(C)]
    #[derive(Debug)]
    struct udmabuf_create {
        memfd: u32,
        flags: u32,
        offset: u64,
        size: u64,
    }
    const UDMABUF_FLAGS_CLOEXEC: u32 = 0x01;

    unsafe impl Ioctl for udmabuf_create {
        type Output = RawFd;

        const IS_MUTATING: bool = false;

        fn opcode(&self) -> Opcode {
            opcode::read_write::<udmabuf_create>(b'u', 0x42)
        }

        fn as_ptr(&mut self) -> *mut rustix::ffi::c_void {
            self as *mut Self as *mut _
        }

        unsafe fn output_from_ptr(
            out: rustix::ioctl::IoctlOutput,
            _extract_output: *mut rustix::ffi::c_void,
        ) -> rustix::io::Result<Self::Output> {
            if out < 0 {
                Err(rustix::io::Errno::from_raw_os_error(!out))
            } else {
                Ok(out)
            }
        }
    }

    let args = udmabuf_create {
        memfd: mem_fd.as_raw_fd() as u32,
        flags: UDMABUF_FLAGS_CLOEXEC,
        offset,
        size,
    };

    unsafe {
        ioctl(fd, args)
            .map(|fd| OwnedFd::from_raw_fd(fd))
            .map_err(|err| io::Error::from_raw_os_error(err.raw_os_error()))
    }
}
