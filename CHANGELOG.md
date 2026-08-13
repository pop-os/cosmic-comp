# [1.38.0](https://github.com/playtron-os/cosmic-comp/compare/v1.37.1...v1.38.0) (2026-08-13)


### Bug Fixes

* **kms:** stop night shift blinking off during display changes ([9dc2347](https://github.com/playtron-os/cosmic-comp/commit/9dc234740c0790082400c06178c67cea4a8c683f))


### Features

* **game-mode:** accept COSMIC_GAME_MODE_ALLOW=* to skip the client check ([bde31b6](https://github.com/playtron-os/cosmic-comp/commit/bde31b6d8d765791e43cd4e0bff8aeefa0f08712))
* **game-mode:** upscale a game with NVIDIA Image Scaling ([7fdc949](https://github.com/playtron-os/cosmic-comp/commit/7fdc94913c487ff4776046b278cfb7507faec49d))
* **game-mode:** wire FSR, expose sharpness, skip a 1:1 upscale ([9643174](https://github.com/playtron-os/cosmic-comp/commit/9643174c18137e5e2ff61276dd1fa3ab539cd447))
* **kms:** apply night shift by CTM where there is no gamma LUT ([4e4aa84](https://github.com/playtron-os/cosmic-comp/commit/4e4aa84fdef80fc7f771ec84df378e3f609a5193))
* **kms:** fade to black before a reboot or poweroff ([bcad3dc](https://github.com/playtron-os/cosmic-comp/commit/bcad3dc96d26d01710cb055037c44c534d398f78))

## [1.37.1](https://github.com/playtron-os/cosmic-comp/compare/v1.37.0...v1.37.1) (2026-08-12)


### Bug Fixes

* fullscreen windows crashing due to frame presentation not acknowledged ([8986598](https://github.com/playtron-os/cosmic-comp/commit/8986598ae2e48ab3f753cab93ebcc6d7af03dde8))

# [1.37.0](https://github.com/playtron-os/cosmic-comp/compare/v1.36.0...v1.37.0) (2026-08-12)


### Bug Fixes

* **kms:** draw the frozen handoff frame at the output's real scale ([0ebbbf2](https://github.com/playtron-os/cosmic-comp/commit/0ebbbf24cff35dcb5393204c8a00f7110b6bbee5))


### Features

* **shell:** fade a fullscreen window out when its client exits ([7bfb53c](https://github.com/playtron-os/cosmic-comp/commit/7bfb53c9bd90bbb0adfa1213f73f0eb73697a6d3))

# [1.36.0](https://github.com/playtron-os/cosmic-comp/compare/v1.35.0...v1.36.0) (2026-08-11)


### Bug Fixes

* **kms:** shut down as soon as the kiosk child exits ([476c103](https://github.com/playtron-os/cosmic-comp/commit/476c1038e179c09c85b788fc5e5e07658315337c))
* **logger:** enable handoff timing marks by default ([da95c97](https://github.com/playtron-os/cosmic-comp/commit/da95c97d25ec1c41fcfdf2540787263a4c30e584))
* **shell:** drop per-frame logging from layer_fade_in_alphas ([12778d9](https://github.com/playtron-os/cosmic-comp/commit/12778d931580023bcc5857c929a36980de1abe71))


### Features

* **kms:** gate the handoff cross-fade on the wallpaper ([8c72714](https://github.com/playtron-os/cosmic-comp/commit/8c7271463646ffc0ff0fd79c8c7a78d21f1adade))
* **kms:** record what blocks the handoff cross-fade ([e771019](https://github.com/playtron-os/cosmic-comp/commit/e771019f416c0aca46574fa3f487e5ff766cead5))
* **render:** default the clear colour to black, overridable by env ([fe20a73](https://github.com/playtron-os/cosmic-comp/commit/fe20a73dcd7b71457226c8bfc6d72794caaccf3a)), closes [#RRGGBB](https://github.com/playtron-os/cosmic-comp/issues/RRGGBB)
* suppress input during the session handoff, and time the handoff ([e4f9dad](https://github.com/playtron-os/cosmic-comp/commit/e4f9dad3d374110d65c7fb19f7b0b8114aac13cd))

# [1.35.0](https://github.com/playtron-os/cosmic-comp/compare/v1.34.2...v1.35.0) (2026-08-11)


### Bug Fixes

* **blur:** give every blur rect its own element state ([9766456](https://github.com/playtron-os/cosmic-comp/commit/9766456d4361688993f28907250a911d3ae881a5))
* **dnd:** clear the drag icon when a drag is cancelled ([80ba206](https://github.com/playtron-os/cosmic-comp/commit/80ba206d23c98775d36b3f538fb5625c632a21de))
* **input:** restore focus when hiding a focused surface ([53c519a](https://github.com/playtron-os/cosmic-comp/commit/53c519a59a4e66bec8bfb32ba362b4736b4b9638))
* **logger:** stop default directives from clobbering RUST_LOG ([549dd9a](https://github.com/playtron-os/cosmic-comp/commit/549dd9aefccedff094ee6fea89ac1cfc54bbdd9a))
* **render:** evict dead layer surfaces from the shadow cache ([435ec1d](https://github.com/playtron-os/cosmic-comp/commit/435ec1d603090304cd42275e7d0f229c4d1cae68))
* **shell:** stop re-leaking client toplevel icons on every refresh ([f857431](https://github.com/playtron-os/cosmic-comp/commit/f85743158fe608cc076193f97b3f110eac420a8f))
* **voice:** never focus an invisible receiver; prune destroyed ones ([7f1c65b](https://github.com/playtron-os/cosmic-comp/commit/7f1c65b94ddbdf6ef747f41a45dc4cd636f9934c))


### Features

* **kms:** enable the session handoff by default ([adcc4ad](https://github.com/playtron-os/cosmic-comp/commit/adcc4adf11861c8aef4c22358f79b146187065ea))
* **kms:** flicker-free session handoff via a frozen scanout frame ([47f70fd](https://github.com/playtron-os/cosmic-comp/commit/47f70fdfae4c70d507995b0fb4b00e97666741a3))
* **lock:** cross-fade the ext-session-lock cover in and out ([09a62e5](https://github.com/playtron-os/cosmic-comp/commit/09a62e50942bdad00af791ba8a5747c7fd32009d))
* memory self-report, opt-in heap profiling, smithay leak fixes ([122942c](https://github.com/playtron-os/cosmic-comp/commit/122942c946a023931ffd4a25676acede24fd4a3f))

## [1.34.2](https://github.com/playtron-os/cosmic-comp/compare/v1.34.1...v1.34.2) (2026-08-05)


### Bug Fixes

* **minimize:** animate the backdrop color with the window ([ff679af](https://github.com/playtron-os/cosmic-comp/commit/ff679af87553c2f68137840d6fb4bd3430ceec6f))
* **theme:** reach minimized windows when applying a theme change ([e2e175a](https://github.com/playtron-os/cosmic-comp/commit/e2e175a015809ce4c871531944c91c0eff31d2f5))

## [1.34.1](https://github.com/playtron-os/cosmic-comp/compare/v1.34.0...v1.34.1) (2026-08-05)


### Bug Fixes

* stop recording a rejected mirror configuration ([76a3cfe](https://github.com/playtron-os/cosmic-comp/commit/76a3cfe350207e91474176c1c5b9a201c93202c8))

# [1.34.0](https://github.com/playtron-os/cosmic-comp/compare/v1.33.0...v1.34.0) (2026-08-04)


### Bug Fixes

* **blur:** keep a backdrop settled while its surface moves ([698bcf7](https://github.com/playtron-os/cosmic-comp/commit/698bcf762e1fb1aec3a30e8fd686318113f2497f))
* **blur:** let a backdrop arriving with its surface appear with it ([77b5e14](https://github.com/playtron-os/cosmic-comp/commit/77b5e14465552f6783d5d41a5daed7ba967e86e9))
* **shadow:** stop squaring the popup shadow's opacity ([36eaba5](https://github.com/playtron-os/cosmic-comp/commit/36eaba59ce35f435cdea1d9e7639d2a86c7f766a))
* **shadow:** wait for a popup to be drawn before shadowing it ([9265ae3](https://github.com/playtron-os/cosmic-comp/commit/9265ae399135e2992a7f36479c0685adf0adaa9a))


### Features

* **shadow:** draw shadows behind popups of ordinary windows ([3baf8b4](https://github.com/playtron-os/cosmic-comp/commit/3baf8b4c6d92ac37de245db638d000434dfa68fd))

# [1.33.0](https://github.com/playtron-os/cosmic-comp/compare/v1.32.0...v1.33.0) (2026-08-04)


### Bug Fixes

* a11y: magnifier: correct Centered/OnEdge focal-point math and refresh focal point at zoom-in ([01c0cb0](https://github.com/playtron-os/cosmic-comp/commit/01c0cb020e9fce991254b1d1e6a6485be6046dde))
* clamp pointer edge after applying the output offset ([8d450d4](https://github.com/playtron-os/cosmic-comp/commit/8d450d497763912c4575c0f28dda661dafd4366c))
* **corner_radius:** check LayerHookId for layer surfaces ([ffeda33](https://github.com/playtron-os/cosmic-comp/commit/ffeda3375a7e60ace6ae64b19432f1f0c1fc1034))
* **corner_radius:** loosen padding size constraint ([b890f7b](https://github.com/playtron-os/cosmic-comp/commit/b890f7b0fa86d087ba2ae4d6cd64ec3a1f8b7cb9))
* **deploy:** accept the sudo password from the environment ([ffdf811](https://github.com/playtron-os/cosmic-comp/commit/ffdf811c3f4987969c9a0aec7c855f33ea0055b7))
* **deploy:** relabel the deployed binary and stop leaking the sudo password ([ac04d80](https://github.com/playtron-os/cosmic-comp/commit/ac04d80a7b5ccc390302908495c5632af50fc611))
* dont play fade in when fullscreening an already maximized window ([f50dfd9](https://github.com/playtron-os/cosmic-comp/commit/f50dfd9a33f073d9e851dfa922b42a936e93b89a))
* ensure X window focus on unmap/map ([5ce21fb](https://github.com/playtron-os/cosmic-comp/commit/5ce21fb4f3de9671d6cb6c4738d9bd02d541a57a))
* fade layer popups in as well as out ([a4a4cc2](https://github.com/playtron-os/cosmic-comp/commit/a4a4cc2806d07a9ed65d05ca5823b253858ed9d2))
* fade layer surfaces in over real content and restore blur parity ([add6c3b](https://github.com/playtron-os/cosmic-comp/commit/add6c3bd0f35254a8cedf8e35521a5dd28bd0d41))
* fade the blur backdrop with its surface ([f4454c2](https://github.com/playtron-os/cosmic-comp/commit/f4454c2b292a2ea575c2ad751ccd85435a279bb1))
* fall back to legacy X11 names when loading pointer themes ([067be10](https://github.com/playtron-os/cosmic-comp/commit/067be10474ff8fc8d0053061e4f8a5f355883de7))
* flicker on wayland context menu pop up ([aabf1cd](https://github.com/playtron-os/cosmic-comp/commit/aabf1cdc08c844ca05339f53fe6c2d56aeeaf59a))
* **game-mode:** don't resolve the caller's pid from inside the method ([4a5965b](https://github.com/playtron-os/cosmic-comp/commit/4a5965b378c430372473dd778c86bd863fd85fcb))
* input to cropped tiled windows should be bound to the tile ([2329cea](https://github.com/playtron-os/cosmic-comp/commit/2329cea5c9f27145cb45aa0d004f14d5020357ea))
* keep the blur capture inside the framebuffer origin ([156558e](https://github.com/playtron-os/cosmic-comp/commit/156558eaf0ff5afe54c78bec83fe5c22295b2469))
* **kms:** don't panic when renderer creation fails after a GPU reset ([9514b49](https://github.com/playtron-os/cosmic-comp/commit/9514b4945edc2656b38a34419c3a8618f490cb17))
* **kms:** gate overlay scanout per-element by buffer GPU node ([012c77e](https://github.com/playtron-os/cosmic-comp/commit/012c77ec81f55f8d149639f77ea7254b43f9e20f))
* **kms:** join surface threads before exit to avoid SIGABRT on shutdown ([53a8d0b](https://github.com/playtron-os/cosmic-comp/commit/53a8d0bea2c1a361c1a0c57b20b4adc4b748c2a0))
* **kms:** skip empty DRM cleanup commit in display_configuration ([24d2900](https://github.com/playtron-os/cosmic-comp/commit/24d29008f0ce00074612b49987e47468db3ef231))
* map_window_request should update pending state ([9789187](https://github.com/playtron-os/cosmic-comp/commit/9789187416810f851b72e75f11b9648515e76794))
* no ceil for scaled outline thickness ([4d370cd](https://github.com/playtron-os/cosmic-comp/commit/4d370cdf92b5d96d78032593947f4ad9eae793bf))
* pad_rect typos ([77f57a6](https://github.com/playtron-os/cosmic-comp/commit/77f57a656d1c03d412f135dd3e5fa647926d1b3d))
* prevent fullscreening freezing compositor ([830e420](https://github.com/playtron-os/cosmic-comp/commit/830e420cc57f181d7f7d2bd61ae2691049869503))
* release active popup grabs on toplevel destruction ([1d72227](https://github.com/playtron-os/cosmic-comp/commit/1d72227a30144c96cd2cd1583fd726b9c9dd717d))
* restart the voice orb's shader clock when it hides ([68f6ff6](https://github.com/playtron-os/cosmic-comp/commit/68f6ff66f112c97bba7f06b6f11beea0f0ea44fe))
* restore corner rounding and blur strength after the upstream merge ([5dd6029](https://github.com/playtron-os/cosmic-comp/commit/5dd602908c568569b4a163c99c0347dd871328cc))
* take into account pending maximized state when recalculating window geometry ([7a7c6b7](https://github.com/playtron-os/cosmic-comp/commit/7a7c6b7fd42e0941b0742314541dcefdacb08fa4))
* use configured blur strength for lock surface and its subsurfaces ([f6d3daf](https://github.com/playtron-os/cosmic-comp/commit/f6d3dafda7790023059237bc717c21a288d552df))
* workaround for Chromium apps breaking below 1.0 scaling ([6108cef](https://github.com/playtron-os/cosmic-comp/commit/6108cef8b368653c51c0bbe6f81132a5a80b5c46))


### Features

* adopt upstream's blur and extend the background-effect protocol ([09297d5](https://github.com/playtron-os/cosmic-comp/commit/09297d5b84eecd17bffebe12bf83bcefbb026fe2))
* attribute blur cache invalidation to the surface causing it ([c71a4c6](https://github.com/playtron-os/cosmic-comp/commit/c71a4c62077b09ead2237f635baed243f4b8b8c8))
* carry blur corner radii per region rect ([9c9d2b2](https://github.com/playtron-os/cosmic-comp/commit/9c9d2b241b62a55a177a9d9774361228873ecacb))
* carry the exact sub-pixel blur area over the wire ([0d1a34a](https://github.com/playtron-os/cosmic-comp/commit/0d1a34ac554a6318f0531012ed578dd33b4cfb66))
* fade in blur rects ([446568e](https://github.com/playtron-os/cosmic-comp/commit/446568e1b4cd240a7605899f236a8a6a3d9ebbbd))
* **game-mode:** implement the scaling modes and the resolution spoof ([d039e40](https://github.com/playtron-os/cosmic-comp/commit/d039e404500dd832d85cc8e6a40f038a19bb1c11))
* **game-mode:** learn the launching process from the bus, off-thread ([bf8e4f6](https://github.com/playtron-os/cosmic-comp/commit/bf8e4f696370ae84f226e99293c9a4510c526d67))
* **game-mode:** only allow authorized clients to drive game mode ([dfe8e08](https://github.com/playtron-os/cosmic-comp/commit/dfe8e08148adfe9815f9397ec0063747c06983ce))
* implement adaptive foreground protocol ([2ec7d6e](https://github.com/playtron-os/cosmic-comp/commit/2ec7d6edc798bef55dc764dc4fe3254e9941cf2d))
* **render:** add the FSR upscaling and sharpening shaders ([5fbf94c](https://github.com/playtron-os/cosmic-comp/commit/5fbf94c9729c203c6ad0e0a4786a6e9b7d1c2bbb))
* **render:** generate the NVIDIA Image Scaling filter coefficients ([a1b7d56](https://github.com/playtron-os/cosmic-comp/commit/a1b7d5662c55974a79ed465928d8051bee108d91))


### Performance Improvements

* cache the blur ping-pong scratch texture ([86c7dc5](https://github.com/playtron-os/cosmic-comp/commit/86c7dc540b326e85ff1afad82b4571192a66fdc6))
* let the renderer cull what an opaque backdrop hides ([120c49b](https://github.com/playtron-os/cosmic-comp/commit/120c49b40cf9d01531ad71c4cfa794fb9d2222e7))
* make blur cheaper and measurable ([05fcefa](https://github.com/playtron-os/cosmic-comp/commit/05fcefa05041f7c0d71fe97b15cf07601ea8ba17))
* reuse the voice orb's render element across frames ([b83ad83](https://github.com/playtron-os/cosmic-comp/commit/b83ad837bd1253a4be82ae59860e95188f5bdcd2))
* skip compositing workspaces that voice mode has faded out ([3e3a2ee](https://github.com/playtron-os/cosmic-comp/commit/3e3a2eeddcbae69e22763864f36b8cd915321886))

# [1.32.0](https://github.com/playtron-os/cosmic-comp/compare/v1.31.0...v1.32.0) (2026-07-30)


### Bug Fixes

* **game-mode:** center launch banners instead of forcing them fullscreen ([24b7b60](https://github.com/playtron-os/cosmic-comp/commit/24b7b60d166fe11676f90159834e556ce4698ac2))
* **game-mode:** don't focus windows game mode will not render ([32989cd](https://github.com/playtron-os/cosmic-comp/commit/32989cd581c5f3f7c3084e017d1900837eb8bc91))
* **game-mode:** drive frame callbacks for the composited overlay ([5c0f46c](https://github.com/playtron-os/cosmic-comp/commit/5c0f46cf61f6831bcf7a2a2b71f4f8898a8a3ea2))
* **game-mode:** filter override-redirect windows and reclaim stolen focus ([397da31](https://github.com/playtron-os/cosmic-comp/commit/397da319c1235019d6b4b4e967f564f5687d0348))
* **game-mode:** keep the game on the output game mode owns ([3aada5b](https://github.com/playtron-os/cosmic-comp/commit/3aada5b813226baffe491855dc504fd980d72780))
* **game-mode:** place a launched game on the game-mode output when it maps ([75f8c55](https://github.com/playtron-os/cosmic-comp/commit/75f8c55a0a3ed57ee0eabfe66d77ff94112c2409))
* **game-mode:** render the controlled base's popups, not the seat's ([68cbc89](https://github.com/playtron-os/cosmic-comp/commit/68cbc894e81cd640195b667beb52ed299acd38c2))
* **game-mode:** route the pointer to a blocking overlay ([496df09](https://github.com/playtron-os/cosmic-comp/commit/496df099255d2cabde5fe4d825bce93737b0dd33))
* **game-mode:** scope strict control to the game's own workspace ([4fedc5f](https://github.com/playtron-os/cosmic-comp/commit/4fedc5f9bac2930c99694315cc4b7b476d13fb07))
* **game-mode:** suppress popups of windows the game workspace hides ([4053102](https://github.com/playtron-os/cosmic-comp/commit/405310275fe1cfe16f12a866030dbdf7f45c18be))
* **game-mode:** undo the presentation scale when hit-testing the game ([e72fb2f](https://github.com/playtron-os/cosmic-comp/commit/e72fb2f320fdeb4d693632bda40a20f762ef6508))
* windows getting stuck in maximized state even while unmaximized ([23c9bfe](https://github.com/playtron-os/cosmic-comp/commit/23c9bfe29643e1afe7e2f0761e427e68824a1dca))


### Features

* **game-mode:** honor base-layer priority from the session manager ([c3d14ab](https://github.com/playtron-os/cosmic-comp/commit/c3d14ab079ea8560cbc1c6d9918a802e0cffe972))
* **game-mode:** let the adopted game's own windows render above it ([f23fe97](https://github.com/playtron-os/cosmic-comp/commit/f23fe97cf0b13c8876175dab4beb947ce08e72ff))
* **game-mode:** rank base candidates so a splash never displaces the game ([d2d887e](https://github.com/playtron-os/cosmic-comp/commit/d2d887e392881412ad1eb122ec681856baaaed56))
* **game-mode:** strict fullscreen control on the game-mode output ([e194349](https://github.com/playtron-os/cosmic-comp/commit/e194349f8bf7e810ab1b58bfee02f4002739368f))
* **logging:** opt-in game-mode trace via COSMIC_GAME_TRACE ([9b102f6](https://github.com/playtron-os/cosmic-comp/commit/9b102f61d2090107c337c46015155f379230462c))

# [1.31.0](https://github.com/playtron-os/cosmic-comp/compare/v1.30.1...v1.31.0) (2026-07-29)


### Bug Fixes

* **blur:** free cached blur textures when their surface goes away ([3dc5e79](https://github.com/playtron-os/cosmic-comp/commit/3dc5e79faafa5c52583633f4e9e9084fc05ee259))
* **blur:** reclaim blur textures for destroyed layer-shell popups ([e968ec1](https://github.com/playtron-os/cosmic-comp/commit/e968ec1244330cb8f471d76df8ed618de28602e9)), closes [tmp/#NNNN](https://github.com/tmp//issues/NNNN)
* clean up blue cache on remove output and handle storing blur group content hash only on successful pass ([9f5f337](https://github.com/playtron-os/cosmic-comp/commit/9f5f337e618ec78d7afc62a624924717ddfcbdb5))
* defer game-mode window mutations out of source dispatch ([1239518](https://github.com/playtron-os/cosmic-comp/commit/12395180c671b9306190f7b1cd2524c046cb4ff5))
* **game-mode:** always fill the output gamescope-style; keep game activated ([234987c](https://github.com/playtron-os/cosmic-comp/commit/234987ca9e95ae65312a919088cd156d1cb59c3c))
* **game-mode:** clean up state on exit + relaunch so a re-launched app fullscreens ([2ff2c51](https://github.com/playtron-os/cosmic-comp/commit/2ff2c51e3d5807cc0f1978c0249e85fb0074d2ec))
* **game-mode:** revert always-fill; keep the fullscreen game scanout-capable ([f7443db](https://github.com/playtron-os/cosmic-comp/commit/f7443db7b6e4c298e3fb62650e8f78dba0e6ac55))
* **iced:** defer IcedElement executor-source removal off the loop thread ([919d924](https://github.com/playtron-os/cosmic-comp/commit/919d924c37222cc61d0fda0e3e02ed5ef85f6504))
* **iced:** keep calloop's Rc state on the event-loop thread ([488aca3](https://github.com/playtron-os/cosmic-comp/commit/488aca3a018d3c209b9c20a542ba5b374116217c))
* keep secondary-output panel blur alive and track wallpaper open animation ([d143739](https://github.com/playtron-os/cosmic-comp/commit/d14373949e1fe20541e9f287cca29aabe1d61e52))
* keep the game-mode window fullscreen + animate launcher exit ([da21b72](https://github.com/playtron-os/cosmic-comp/commit/da21b7262baf563c92dbfea52eaf60c531eccc59))
* **motion:** source panel slide from ease_out_expo, keep ease_spring a spring ([31b6350](https://github.com/playtron-os/cosmic-comp/commit/31b6350093c757cf69b7103751e6e1d8c5bac265))
* remove hard coded F23 capture for voice input ([80aa149](https://github.com/playtron-os/cosmic-comp/commit/80aa149aff6f31794209aeaf129f4d5f44e41b1c))
* report game-mode display caps from the game's output ([5d91b1f](https://github.com/playtron-os/cosmic-comp/commit/5d91b1fac24fa1aab39b600ebdc5213f6abe51e6))
* rounded corner clipping ([ab319bd](https://github.com/playtron-os/cosmic-comp/commit/ab319bd9119ea879dd7014df3940cf5f425c63ce))
* update smithay to fix x11 vkcube crashing compositor ([bded512](https://github.com/playtron-os/cosmic-comp/commit/bded512795eac812a5279ce9937292dbc9d29908))


### Features

* **debug:** add opt-in parking_lot deadlock detection ([5163425](https://github.com/playtron-os/cosmic-comp/commit/516342531044c57990967bf9eafae6dfa3dff40d))
* **game-mode:** composite a Wayland launcher/overlay over the fullscreen game ([d4ed95b](https://github.com/playtron-os/cosmic-comp/commit/d4ed95bca72797f622bcd67c3893f80ec9835b11))
* **game-mode:** cross-fade launcher<->game workspace switches ([812dddc](https://github.com/playtron-os/cosmic-comp/commit/812dddcf9ffa86b52a842c7242c63134512ef1fa))
* **game-mode:** emit launcher key (Super) as raw down/up events ([72707e4](https://github.com/playtron-os/cosmic-comp/commit/72707e4ea6bedffb015edbc505df2bcdf811a60f))
* **game-mode:** reroute bare-Super launcher key to Grid; add LauncherKeyPressed signal ([ea7c64f](https://github.com/playtron-os/cosmic-comp/commit/ea7c64fca07a3655fbc7e6366b4580bbc4ddf440))
* **game-mode:** scanout-safe gamescope upscaler for sub-output games ([9f37c15](https://github.com/playtron-os/cosmic-comp/commit/9f37c15ea6f0dea8745b8043761ea5fbe1ae93c8))
* make default output scale a bit bigger and fix workspaces appearing even when they are disabled ([9242d35](https://github.com/playtron-os/cosmic-comp/commit/9242d35b5f0a57ae32784de19d6826ecf4a81865))
* make the launcher a game-mode target + exclusive game output ([5db7d65](https://github.com/playtron-os/cosmic-comp/commit/5db7d65e020551836ece7442b487a3ba88e03125))
* re-resolve the game surface when STEAM_GAME is retagged ([ec7aed8](https://github.com/playtron-os/cosmic-comp/commit/ec7aed8b44e970250fb1edd3935d789e6ee97fef))
* support activating layershell via xdg activation ([3fccbb1](https://github.com/playtron-os/cosmic-comp/commit/3fccbb106e6cd2260af9013852473a29be5ea193))
* support per rect corner radii for blur protocol ([d3de4bc](https://github.com/playtron-os/cosmic-comp/commit/d3de4bca00c323488959c5ecc50d5d8f4e7e0c2f))
* workspace-based game mode (dedicated clean workspace per app) ([629969c](https://github.com/playtron-os/cosmic-comp/commit/629969c44cc9d00f8de396987210de546f9cd46e))

## [1.30.1](https://github.com/playtron-os/cosmic-comp/compare/v1.30.0...v1.30.1) (2026-07-23)


### Bug Fixes

* keep new floating windows centered when a closing window is still mapped ([c55e683](https://github.com/playtron-os/cosmic-comp/commit/c55e683cc679d60ec7da22e1a53076cfa9fc1c39))
* never show an empty scene as the backdrop blur capture ([68749b4](https://github.com/playtron-os/cosmic-comp/commit/68749b434bb9931f96c8826c668ff1144df555c4))

# [1.30.0](https://github.com/playtron-os/cosmic-comp/compare/v1.29.2...v1.30.0) (2026-07-23)


### Features

* persist clipboard contents after the source client exits ([1d87eb2](https://github.com/playtron-os/cosmic-comp/commit/1d87eb255cfab0658ce6c610d675f12819d4ec02))

## [1.29.2](https://github.com/playtron-os/cosmic-comp/compare/v1.29.1...v1.29.2) (2026-07-23)


### Bug Fixes

* clamp window positioning on right/bottom as well ([ad727ea](https://github.com/playtron-os/cosmic-comp/commit/ad727ea2ef6b266c0022b2969ec5868651ea1e7e))

## [1.29.1](https://github.com/playtron-os/cosmic-comp/compare/v1.29.0...v1.29.1) (2026-07-21)


### Bug Fixes

* **input:** honor keyboard-shortcuts inhibitor for voice/chat Super key ([a700075](https://github.com/playtron-os/cosmic-comp/commit/a70007570fd7c8a1f67b6cbdf8856969aca1995e))
* restore resizing after dragging a maximized window away to unmaximize (HUM-238) ([fc524ce](https://github.com/playtron-os/cosmic-comp/commit/fc524cee7b523aff9c08b8597f140cf1b8f5bdce))

# [1.29.0](https://github.com/playtron-os/cosmic-comp/compare/v1.28.9...v1.29.0) (2026-07-20)


### Features

* implement safe gpu reset detection and handling to avoid restarting compositor ([86bf498](https://github.com/playtron-os/cosmic-comp/commit/86bf4986aa6f76c5a9f2cabdda5bd13ac636352e))

## [1.28.9](https://github.com/playtron-os/cosmic-comp/compare/v1.28.8...v1.28.9) (2026-07-18)


### Bug Fixes

* make x11 configs async in set geometry to prevent freeze during x11 window close ([77ec1e7](https://github.com/playtron-os/cosmic-comp/commit/77ec1e79b578ca19cf2a9390947bcda39932c768))

## [1.28.8](https://github.com/playtron-os/cosmic-comp/compare/v1.28.7...v1.28.8) (2026-07-13)


### Bug Fixes

* do not animate offset/scale for full output layershell surfaces ([d9c7f05](https://github.com/playtron-os/cosmic-comp/commit/d9c7f054cf41a7ec31c8f4224dfcd725fcc5aed7))
* focus tapped window on touchscreen (HUM-165) ([5f93c54](https://github.com/playtron-os/cosmic-comp/commit/5f93c54076aebba9237c6d52bbe320713b7d8482))
* hide cursor on touchscreen input (HUM-166) ([07a1bc7](https://github.com/playtron-os/cosmic-comp/commit/07a1bc7e28e68e4a27b795b07312351d2aa91951))

## [1.28.7](https://github.com/playtron-os/cosmic-comp/compare/v1.28.6...v1.28.7) (2026-07-09)


### Bug Fixes

* fix crash on active_num running before workspace is set ([13440fb](https://github.com/playtron-os/cosmic-comp/commit/13440fb2b8ab70edc4d1281a3e16290fa4567778))

## [1.28.6](https://github.com/playtron-os/cosmic-comp/compare/v1.28.5...v1.28.6) (2026-07-08)


### Bug Fixes

* prevent broadcasting get_head reads to every client ([c078d6d](https://github.com/playtron-os/cosmic-comp/commit/c078d6d3a7af595d9df8751081df16aace6bfc72))
* prevent crash by mapping rendering types correctly in post process ([bca8ea6](https://github.com/playtron-os/cosmic-comp/commit/bca8ea60681fc43191d7c94cc3ad79b9c19d6796))
* use ObjectId in layer dismiss protocol to prevent collisions ([d2e6964](https://github.com/playtron-os/cosmic-comp/commit/d2e6964225f5e2aa302b449f14ce8fca8deea2ce))

## [1.28.5](https://github.com/playtron-os/cosmic-comp/compare/v1.28.4...v1.28.5) (2026-07-02)


### Bug Fixes

* correctly unmap x11 windows instead of re-queueing them ([2995f38](https://github.com/playtron-os/cosmic-comp/commit/2995f3819006cc48c29c1c7879c4186a70ec966e))
* guard against zombie x11 windows mapping ([1495b39](https://github.com/playtron-os/cosmic-comp/commit/1495b39ecfe6d6d27cc278e51c46a22f728f863d))
* internal laptop screen stuck on vendor logo with external monitor ([a9f2427](https://github.com/playtron-os/cosmic-comp/commit/a9f2427c00dddbd78ddc3990c84751d6993c3f8d))
* keep windows visible until overview commits a buffer ([6507682](https://github.com/playtron-os/cosmic-comp/commit/650768211fbfa3bcde7f7b52c152cc630dbe6404))
* merge conflict fixes from last commit ([562f299](https://github.com/playtron-os/cosmic-comp/commit/562f2997048ac6127a13f0b70ed5784d3add8158))
* **xwayland:** eliminate black-frame flash by mapping X11 windows on first rendered frame ([3adfa23](https://github.com/playtron-os/cosmic-comp/commit/3adfa230926a0cbb9ea169df5db7f2df21055e35))


### Reverts

* "fix: follow the focus after alt+tab to another output" ([9d52653](https://github.com/playtron-os/cosmic-comp/commit/9d52653d5c9d45716dbc333f99f5ef7e25549bf8))

## [1.28.4](https://github.com/playtron-os/cosmic-comp/compare/v1.28.3...v1.28.4) (2026-06-29)


### Bug Fixes

* correctly scale blur regions during animation ([dc3c957](https://github.com/playtron-os/cosmic-comp/commit/dc3c957b00a7380c39d8b3d067495b7838feea02))

## [1.28.3](https://github.com/playtron-os/cosmic-comp/compare/v1.28.2...v1.28.3) (2026-06-29)


### Bug Fixes

* lighten requires on rpm spec ([e057355](https://github.com/playtron-os/cosmic-comp/commit/e05735590d513babf6760fc871ef0a7b3d9a0902))

## [1.28.2](https://github.com/playtron-os/cosmic-comp/compare/v1.28.1...v1.28.2) (2026-06-29)


### Bug Fixes

* make chat panel drag sash use primary theme color ([e23861b](https://github.com/playtron-os/cosmic-comp/commit/e23861b6856c530a8fccdb81cefd4cfe1c828af8))

## [1.28.1](https://github.com/playtron-os/cosmic-comp/compare/v1.28.0...v1.28.1) (2026-06-29)


### Bug Fixes

* **kms:** gate overlay scanout per-element by buffer GPU node ([ad63a76](https://github.com/playtron-os/cosmic-comp/commit/ad63a76714787cab0ba2e91c76a2d5ab7a6f53f0))

# [1.28.0](https://github.com/playtron-os/cosmic-comp/compare/v1.27.2...v1.28.0) (2026-06-28)


### Features

* per-app and per-window Steam atom integration for game mode ([abdc090](https://github.com/playtron-os/cosmic-comp/commit/abdc090a27612774d39ef51647a8b6b4b87c6760))
* remove libdisplay-info dynamic lib requirement to make it compatible with fedora 44 ([436ad2e](https://github.com/playtron-os/cosmic-comp/commit/436ad2e8f6747657a20b42a75f0b973bb4e2891a))

## [1.27.2](https://github.com/playtron-os/cosmic-comp/compare/v1.27.1...v1.27.2) (2026-06-27)


### Bug Fixes

* update deps ([10058f4](https://github.com/playtron-os/cosmic-comp/commit/10058f4340c926aa8c9ceb6cafb1135a51581322))

## [1.27.1](https://github.com/playtron-os/cosmic-comp/compare/v1.27.0...v1.27.1) (2026-06-27)


### Bug Fixes

* change default Super binding to StartMenu ([9b7f8a5](https://github.com/playtron-os/cosmic-comp/commit/9b7f8a5e7b51c38c89a41b040813fd2f233c24b6))
* fix local RPM build and update deps ([a8f2cac](https://github.com/playtron-os/cosmic-comp/commit/a8f2cac579d8f41b8d65b20e9689ab92b4c153fb))
* **kms:** clear stale blur textures on session resume (VT-switch flicker) ([1148303](https://github.com/playtron-os/cosmic-comp/commit/114830358442f17918beee7cf570f725a334a210))
* run fmt and also fix the double pointer speed issue ([c459c1e](https://github.com/playtron-os/cosmic-comp/commit/c459c1e336e650ccd9aaa679e2c7ba5d00f07b84))
* update dependencies to add System action ([5242e77](https://github.com/playtron-os/cosmic-comp/commit/5242e7766ae0b1386f4de96f1c6d2a84f82c78ff))

# [1.27.0](https://github.com/playtron-os/cosmic-comp/compare/v1.26.0...v1.27.0) (2026-06-26)


### Bug Fixes

* add git hash to .cargo/config when vendoring ([8f5f7c8](https://github.com/playtron-os/cosmic-comp/commit/8f5f7c897deaf0fac947ed1295692a35f318c8e6))
* break from loop instead of unwrapping ([1dab42e](https://github.com/playtron-os/cosmic-comp/commit/1dab42ed566cbfba617cc764c6f1d1ad4a5160c7))
* clean up pending_windows for surfaces that were never mapped ([3e84968](https://github.com/playtron-os/cosmic-comp/commit/3e84968dacb6a50a82ef5e35c0ac373c38172681))
* consider SSD/tab height when calculating last server size ([fdf015c](https://github.com/playtron-os/cosmic-comp/commit/fdf015cbcf4f71c1a08d3158aa94c49b4ce37592))
* **dbus:** Block on creation of zbus connection to avoid deadlock ([5153d49](https://github.com/playtron-os/cosmic-comp/commit/5153d497e78c8e16ce8689eb7f03d2b4d0b250fd))
* debug build pinning panic ([52b3f93](https://github.com/playtron-os/cosmic-comp/commit/52b3f930a88db6cdde73dbdd65a056cdc0efa40f))
* draw the focused window border for the current window only ([0312f9a](https://github.com/playtron-os/cosmic-comp/commit/0312f9a2017de92abf82386c979bee74d7a0255d))
* Ellipsize tab_text and use text widget instead of custom impl ([01f1785](https://github.com/playtron-os/cosmic-comp/commit/01f1785af51b47c3fd5051f5f5c5b3e4c4ab26cd))
* **floating:** allow remapping minimized windows ([aff506b](https://github.com/playtron-os/cosmic-comp/commit/aff506bb7bab5d3280b1c910c096b3dd1bf3098c))
* follow the focus after alt+tab to another output ([56f84fb](https://github.com/playtron-os/cosmic-comp/commit/56f84fba2dc0ff7de191e48e56f29d92ba50e668))
* Fullscreen request unreachable code crash ([09bca7a](https://github.com/playtron-os/cosmic-comp/commit/09bca7a5752908cefb71b85e0e720994173ad3ed))
* fullscreen x11 games opening as floating/tiled windows ([200074e](https://github.com/playtron-os/cosmic-comp/commit/200074e580cbefadf20d5ee9d492846ab0d8e498))
* **headerbar:** double click handling & resize only on press ([b28a435](https://github.com/playtron-os/cosmic-comp/commit/b28a435b18a3c1a8dfb8dbf781b40610aca03604))
* iced widget event handling ([2e08a87](https://github.com/playtron-os/cosmic-comp/commit/2e08a871bebfdeaef175c9371fd66029a0e01ecb))
* ignore configure requests from fullscreen windows ([451b831](https://github.com/playtron-os/cosmic-comp/commit/451b8319d633e50d729c60a6719a4dd57d751574))
* **keyboard:** Fix keyboard shortcuts for non-Latin layouts ([110c11e](https://github.com/playtron-os/cosmic-comp/commit/110c11eff95c317f8295c8acdaab8e367941bbb9))
* **kms:** don't direct-scanout client buffers across GPUs ([a834229](https://github.com/playtron-os/cosmic-comp/commit/a834229f59efaeaebcf220941aea1a821c9d6c8b))
* **kms:** keep blur-backed surfaces off overlay planes (idle->scroll blink) ([96f56e0](https://github.com/playtron-os/cosmic-comp/commit/96f56e0f29bed5a79633a68a92d77c732ed6bdcb)), closes [hi#FPS](https://github.com/hi/issues/FPS)
* **license:** link public icetron-p instead of proprietary icetron ([c23299e](https://github.com/playtron-os/cosmic-comp/commit/c23299e5dc7511e94b5d07a22e898da2ae7699a7))
* **magnifier:** smooth mouse wheel zoom and respect natural scroll direction ([92fcceb](https://github.com/playtron-os/cosmic-comp/commit/92fcceba54cb1a8ce76ab5a62a6a4602b185580b))
* only move the grabbed window if a new zone is chosen ([1dfc948](https://github.com/playtron-os/cosmic-comp/commit/1dfc948f1ebf2995fcbf9efe570103f9a46cf7e4))
* pass the full window size to xwayland ([51dd3bc](https://github.com/playtron-os/cosmic-comp/commit/51dd3bc66fe8d189264062d1e350abb96c1021d6))
* **perf:** cold-start badge, softer un-clipped badge shadow, voice-chord guard, default target ([673b940](https://github.com/playtron-os/cosmic-comp/commit/673b940a91f196cdcc9f0cd8c30d4afe362a002d))
* Prevent system hang on double Caps Lock press ([21679f2](https://github.com/playtron-os/cosmic-comp/commit/21679f215acfa7ae86072362c5d91b0cc4ed1fcd))
* Remove redundant configure in popup reposition_request ([f865ad7](https://github.com/playtron-os/cosmic-comp/commit/f865ad7241a772a62b558866ab2dc87e99eb6383))
* restore snapped windows as snapped after maximize/minimize ([b5186ef](https://github.com/playtron-os/cosmic-comp/commit/b5186ef21a74f9399714d2bac03ca016cd5820b2))
* restore the window to where it was before the drag ([7c02df2](https://github.com/playtron-os/cosmic-comp/commit/7c02df250e8f17afd406dbc1460d131fb2b13777))
* set_rectangle leak ([aac1e19](https://github.com/playtron-os/cosmic-comp/commit/aac1e19f08a016ade349569fbf8c0305761de20b))
* skip dead windows in floating set_output ([31f317d](https://github.com/playtron-os/cosmic-comp/commit/31f317d16bcbe7cf81b185392d9bde4c1430669b))
* text position in stack ([ba84ee9](https://github.com/playtron-os/cosmic-comp/commit/ba84ee9324fc113b85dece30f2c6d113fe0a50b4))
* update deps ([5f53b14](https://github.com/playtron-os/cosmic-comp/commit/5f53b144027cb17d83294a225dcb5e6b524f2e93))
* update deps ([4189719](https://github.com/playtron-os/cosmic-comp/commit/4189719b249750e699531833f726e0f2f406869a))
* update test ([ab53861](https://github.com/playtron-os/cosmic-comp/commit/ab538615da46274a7b1fda8ca17f589e9d5786c9))
* windows are clipped in overview mode causing visual artifact ([a623f18](https://github.com/playtron-os/cosmic-comp/commit/a623f18c25471922fdb991c69bf3d7c46c42075b))


### Features

* add --no-xwayland argument for running without Xwayland ([98cca4f](https://github.com/playtron-os/cosmic-comp/commit/98cca4f3b7e18e55e1d64537bc9782e259e10f8c))
* add chatpanel system action and spotlight ([9bec9c4](https://github.com/playtron-os/cosmic-comp/commit/9bec9c4738df11e2e98bd1d50e99a908b22c2acb))
* add config option for xdg activation behavior ([2f7c34f](https://github.com/playtron-os/cosmic-comp/commit/2f7c34f29ae8d1d5b7d147bdb69ae443bd630e98))
* allow naming pinned workspaces ([db0b1af](https://github.com/playtron-os/cosmic-comp/commit/db0b1afeb5405d67c2993c5e5c4d15d82a120cb4))
* **input:** bind XF86LaunchA to workspace overview ([d70d2a7](https://github.com/playtron-os/cosmic-comp/commit/d70d2a7be946dd2a192ed5dc7b35e5939eeb90d0))
* separate logind feature from systemd ([821b431](https://github.com/playtron-os/cosmic-comp/commit/821b431973fbc1ce2fae1c2a347eadbbae833246)), closes [#2473](https://github.com/playtron-os/cosmic-comp/issues/2473)
* support multiple fullscreen windows per workspace ([28ef6bd](https://github.com/playtron-os/cosmic-comp/commit/28ef6bdbc8f5a9040346a1a8f7293cbbdb2943bc))
* **tiling-exceptions:** add exception for Slack huddle preview ([5ca8cc2](https://github.com/playtron-os/cosmic-comp/commit/5ca8cc2cdf7c753580a686a58092eb497f326a6f))
* **tiling-exceptions:** add exception for Thunderbird message compose dialog ([f95b626](https://github.com/playtron-os/cosmic-comp/commit/f95b62635d65aacfe56b46de4e028cc77c0ab507))


### Performance Improvements

* **toplevel_info:** optimize send_toplevel_to_client ([d4c5714](https://github.com/playtron-os/cosmic-comp/commit/d4c5714c108aef348da66a8a0175c42898eb643e))

# [1.26.0](https://github.com/playtron-os/cosmic-comp/compare/v1.25.0...v1.26.0) (2026-06-24)


### Bug Fixes

* **perf:** exclude idle gaps from drops/jank/percentiles ([239e51f](https://github.com/playtron-os/cosmic-comp/commit/239e51faa4363d62a3316009e96d4456882976cf))
* **perf:** honest headroom (frame-production time) + per-phase gpu busy ([9d89a9d](https://github.com/playtron-os/cosmic-comp/commit/9d89a9d6561411357d33a82a1488fdd9002e9156))
* **perf:** point cold-start note to F11; average gpu_busy over each phase ([63aefd1](https://github.com/playtron-os/cosmic-comp/commit/63aefd1df86d52ab83dfc83fb2c4ed8329d8ec03))


### Features

* **perf:** iced capture badge, active-fps metric, cold-start diagnostics ([fdfb289](https://github.com/playtron-os/cosmic-comp/commit/fdfb289b26c0a1b8acd6c728ad8cbef1e9f13c13))
* **perf:** two-phase F12 capture (idle + forced max-fps) with sectioned report ([d8d05f5](https://github.com/playtron-os/cosmic-comp/commit/d8d05f55cfb51b2ed6b75e12cb80bc2996046186))

# [1.25.0](https://github.com/playtron-os/cosmic-comp/compare/v1.24.0...v1.25.0) (2026-06-24)


### Bug Fixes

* airtight side-panel slide animation (blink, lag, phantom resize) ([c052f79](https://github.com/playtron-os/cosmic-comp/commit/c052f79db6d45e864ad8f2cc42bad2582e765e4e))
* fix freezing when dragging chat panel ([5ad9949](https://github.com/playtron-os/cosmic-comp/commit/5ad99495b70f2c30ff1d25e92ae0813b82ba21f5))
* **perf:** require Shift in hotkey chord; avoid TTY/voice conflicts ([bb1de63](https://github.com/playtron-os/cosmic-comp/commit/bb1de639641cc978b5f84ae307c9c2988121f349))
* revert forced layer arrangement for panel ([e4b351f](https://github.com/playtron-os/cosmic-comp/commit/e4b351f3a63032a1b85263d8283a6087ddc14626))


### Features

* add support for xdg toplevel icon protocol and fix easing and duration for chat panel animation ([e935e20](https://github.com/playtron-os/cosmic-comp/commit/e935e20668461b7468dc02460804a6560bf26709))
* fade+rise default layer transition + auto-size surface pinning ([fa808cd](https://github.com/playtron-os/cosmic-comp/commit/fa808cd8c7133de4db25ba5a3b8baa99c3f79050))
* implement layer edge resize protocol ([779bc7f](https://github.com/playtron-os/cosmic-comp/commit/779bc7fddbe3c13a89a5d30144def9dcfadde5bb))
* implement layer surface placement protocol and update smithay ([641d458](https://github.com/playtron-os/cosmic-comp/commit/641d458b08ffe9e5fb15bfc6f06a71eb6d5c4c56))
* implement layer usable area protocol so clients can know what usable area inside of exclusive zones are ([b668f66](https://github.com/playtron-os/cosmic-comp/commit/b668f667f41ac1e6400615f737609e7dbefe9a38))
* native exclusive game mode via one.playtron.GameMode ([bf51aa7](https://github.com/playtron-os/cosmic-comp/commit/bf51aa732adfe9a0f2aa2abb6f52c7c46d842c7a))
* **perf:** completeness metrics, zero-cost armed capture, capture badge ([868061a](https://github.com/playtron-os/cosmic-comp/commit/868061a842462a9b370cf4e3cfa2d02a8d8216a0))
* **perf:** on-demand UI performance reports via hotkeys ([957afc3](https://github.com/playtron-os/cosmic-comp/commit/957afc38a0e27f0ed6dfbe8ee3608e2ee89a1cca))

# [1.24.0](https://github.com/playtron-os/cosmic-comp/compare/v1.23.0...v1.24.0) (2026-06-16)


### Bug Fixes

* fix custom corner radius for surfaces ([49e0a46](https://github.com/playtron-os/cosmic-comp/commit/49e0a46410916b2ac5a6e97d827f2c4c1113a271))
* keep panel anchored edge pinned during maximize/restore buffer lag ([5d962cb](https://github.com/playtron-os/cosmic-comp/commit/5d962cb6b61690957878df279776193174c61275))
* **output:** compute panel diagonal with Pythagoras, not area ([a6fdf79](https://github.com/playtron-os/cosmic-comp/commit/a6fdf79dc8d07541f2320b2665e152937b5f2b87))
* **output:** reject bogus non-zero EDID physical sizes ([6caa581](https://github.com/playtron-os/cosmic-comp/commit/6caa5817ec772fc93975a2eeb87d350df35eb3e2)), closes [hi#DPI](https://github.com/hi/issues/DPI)
* support blur for popups ([f8bc8ef](https://github.com/playtron-os/cosmic-comp/commit/f8bc8ef9de665343704e0be3a4a266f7f513947e))
* update deps and watch active theme changes ([2981d3f](https://github.com/playtron-os/cosmic-comp/commit/2981d3f4fc041a486d68a9a4fa7e958c90b0a6f5))
* update smithay to fix surface arrangement with chat panel ([f1e0533](https://github.com/playtron-os/cosmic-comp/commit/f1e05335c062363634276b66624496063dc33d5f))
* use correct tokens for header title syle and fix smithay rev ([7dee8ff](https://github.com/playtron-os/cosmic-comp/commit/7dee8ffa1a56e129819c3358c03d2c34b183c3b0))
* wait for panel buffer to settle before finishing resize ([293ef7b](https://github.com/playtron-os/cosmic-comp/commit/293ef7b05ca47db169e8274cb35e4c0f5db3d3c7))


### Features

* add support for modal animations for surfaces with namespace "agentos-modal" ([6883290](https://github.com/playtron-os/cosmic-comp/commit/6883290b501ded30572b8c567959fbd3a7ff0c10))
* compositor-driven resize, maximize, and z-order for the chat panel ([7b5d8bb](https://github.com/playtron-os/cosmic-comp/commit/7b5d8bba739486e1c5cc36d34d8954fa712c7c18))
* fade out xdg popups as well ([b05de6b](https://github.com/playtron-os/cosmic-comp/commit/b05de6b63ee748d28fa152dddb1ecd41a60b3a1e))
* **output:** expose recommended display scale for settings/FTUE ([9e1991f](https://github.com/playtron-os/cosmic-comp/commit/9e1991fe78395dc52430464dfbcaf56641544393))
* support layer surface visibility transition ([76fac05](https://github.com/playtron-os/cosmic-comp/commit/76fac050d54e9f146b3f7629689a94c9a0f928f1))
* update smithay to support arranging horizontal surfaces before vertical, fix xdg popups to respect exclusive zone, and improve cross fade transition when opening/closing panel ([e93cc76](https://github.com/playtron-os/cosmic-comp/commit/e93cc7680d0c3b7aaf3a1a45cc5c40b51e5c61aa))

# [1.23.0](https://github.com/playtron-os/cosmic-comp/compare/v1.22.0...v1.23.0) (2026-06-08)


### Bug Fixes

* hide surfaces on spawn when needed ([ee911af](https://github.com/playtron-os/cosmic-comp/commit/ee911af2088c5140ae986230d0895d3444891450))


### Features

* implement ability to fine tune border, tint, and add saturate feature in blur protocol ([ce59ba0](https://github.com/playtron-os/cosmic-comp/commit/ce59ba0b5990504b74154af23c0c44eef505b6c6))

# [1.22.0](https://github.com/playtron-os/cosmic-comp/compare/v1.21.3...v1.22.0) (2026-06-01)


### Features

* add support to launch agentos-settings instead of cosmic-settings ([9dbdcb1](https://github.com/playtron-os/cosmic-comp/commit/9dbdcb191fd41bcee4cc5e8ee8ae9f692e47c1d0))

## [1.21.3](https://github.com/playtron-os/cosmic-comp/compare/v1.21.2...v1.21.3) (2026-05-29)


### Bug Fixes

* update dependencies ([ebd45d9](https://github.com/playtron-os/cosmic-comp/commit/ebd45d920766a793f7774db22a5dcb7a630cb87e))

## [1.21.2](https://github.com/playtron-os/cosmic-comp/compare/v1.21.1...v1.21.2) (2026-05-28)


### Bug Fixes

* re-enable transparent SSD header ([c6e043b](https://github.com/playtron-os/cosmic-comp/commit/c6e043bcd26d43d67ed4ff6f717f8573e8474375))
* update icetron and themes libs ([ef35766](https://github.com/playtron-os/cosmic-comp/commit/ef35766bc56086d777df60449a2e64c3baf6d08c))
* update icetron themes to fix theme watching in system dirs ([86fc1d7](https://github.com/playtron-os/cosmic-comp/commit/86fc1d75c15823ffba482c23035db31de0464adf))
* update icetron themes to fix theming issues ([42b6b79](https://github.com/playtron-os/cosmic-comp/commit/42b6b79533c793cdf787c25927985c1773f7574f))
* update icetron to fix SSD header not showing border ([0ad8c84](https://github.com/playtron-os/cosmic-comp/commit/0ad8c84114265f1eedd38a80d1695e627cd80f84))

## [1.21.1](https://github.com/playtron-os/cosmic-comp/compare/v1.21.0...v1.21.1) (2026-05-27)


### Bug Fixes

* issues with SSD header transparency and blur effect ([418c821](https://github.com/playtron-os/cosmic-comp/commit/418c8215a79faa81f18b7fae692ae479b84af975))

# [1.21.0](https://github.com/playtron-os/cosmic-comp/compare/v1.20.1...v1.21.0) (2026-05-27)


### Bug Fixes

* shrink zone-filling windows on drag to prevent stuck maximized state ([28ce53f](https://github.com/playtron-os/cosmic-comp/commit/28ce53fc21a12e7f3c7de8b1d33937399a9e3df8))
* update deps ([4ac237f](https://github.com/playtron-os/cosmic-comp/commit/4ac237f8b60bbbab8e48815ff952fa8f318c4b32))


### Features

* panel slide animation with window size save/restore ([324e1df](https://github.com/playtron-os/cosmic-comp/commit/324e1dfde7046bb9bddbd8ada8d5142d3084cc7c))
* square corners for windows filling the output zone ([3f7c543](https://github.com/playtron-os/cosmic-comp/commit/3f7c543993db6196b25dd5a9b5bd0cd8937591e1))

## [1.20.1](https://github.com/playtron-os/cosmic-comp/compare/v1.20.0...v1.20.1) (2026-05-12)


### Bug Fixes

* add missing bg to SSD header ([8c534d0](https://github.com/playtron-os/cosmic-comp/commit/8c534d0eb2216f094f38321480bd1242f075a56f))

# [1.20.0](https://github.com/playtron-os/cosmic-comp/compare/v1.19.1...v1.20.0) (2026-05-12)


### Bug Fixes

* use inset border for SSD windows to eliminate top edge gap ([25ddbda](https://github.com/playtron-os/cosmic-comp/commit/25ddbdac8b85e286e54b6a15450de16f1a60d23d))


### Features

* use dynamic theme tokens for window border and shadow rendering ([dbeccdf](https://github.com/playtron-os/cosmic-comp/commit/dbeccdf7b271edf5d25fd7a14ed5dcfdc7ffccae))

## [1.19.1](https://github.com/playtron-os/cosmic-comp/compare/v1.19.0...v1.19.1) (2026-05-12)


### Bug Fixes

* use theme radius for SSD windows so theme changes update corner radius ([fe8a55c](https://github.com/playtron-os/cosmic-comp/commit/fe8a55c53d33aa7ce8054c46e16ef4ad0a29ab52))

# [1.19.0](https://github.com/playtron-os/cosmic-comp/compare/v1.18.1...v1.19.0) (2026-05-11)


### Bug Fixes

* propagate fractional scale to CSD subsurfaces on HiDPI ([cdb261d](https://github.com/playtron-os/cosmic-comp/commit/cdb261d22c7fb0dac49b906d8237f52e017d4057))
* update customized-window-decorations example for current API ([57b71ce](https://github.com/playtron-os/cosmic-comp/commit/57b71cead6a009f138a3122c4bedb7ca34d131e7))
* update icetron and update dockerfile rust version ([ee17af2](https://github.com/playtron-os/cosmic-comp/commit/ee17af29d418df71923a5c1a8ac4ee41129f88c4))
* use client specified corner radius for windows correctly ([12f5325](https://github.com/playtron-os/cosmic-comp/commit/12f53253ee5bb0a885f9683fb288f22491524ec3))
* use conditional border rendering for SSD vs CSD windows ([5a2464f](https://github.com/playtron-os/cosmic-comp/commit/5a2464fd7648e806816946229d13be611feb2d8a))


### Features

* icetron theme system, tiling runtime config, SSD corner AA ([d85b1ae](https://github.com/playtron-os/cosmic-comp/commit/d85b1aef7763bccb7181a3ab377028a88badb8a5))
* theme-configurable SSD header background and blur ([ad8dd22](https://github.com/playtron-os/cosmic-comp/commit/ad8dd2228b2e3f4da11591bd43028fde807e836c))
* update to latest smithay ([1184174](https://github.com/playtron-os/cosmic-comp/commit/11841744fafe88fa33da7ce89a9441baca5beff0))

## [1.18.1](https://github.com/playtron-os/cosmic-comp/compare/v1.18.0...v1.18.1) (2026-04-24)


### Bug Fixes

* update libcosmic for header bar and change size to 47px ([6273d1d](https://github.com/playtron-os/cosmic-comp/commit/6273d1dfd9d0d7a35174b6c0418efa55e161c64d))

# [1.18.0](https://github.com/playtron-os/cosmic-comp/compare/v1.17.0...v1.18.0) (2026-04-23)


### Features

* add forced title/icon properties support in .desktop files to override it for chromium apps ([116e082](https://github.com/playtron-os/cosmic-comp/commit/116e08236b02583067cd1ac763b21bafd25ed2e0))

# [1.17.0](https://github.com/playtron-os/cosmic-comp/compare/v1.16.1...v1.17.0) (2026-04-22)


### Features

* add blur radius support and simplify shadow rendering ([5aa0e5d](https://github.com/playtron-os/cosmic-comp/commit/5aa0e5d84db86f1fef09545609c8b8a6d8b203f8))
* **blur:** per-region blur rendering with surface-bounds clamping ([12044c4](https://github.com/playtron-os/cosmic-comp/commit/12044c432c3caa6cbd6bfe87e1320d7d9f29432f))

## [1.16.1](https://github.com/playtron-os/cosmic-comp/compare/v1.16.0...v1.16.1) (2026-04-15)


### Bug Fixes

* dont change exclusive focus every frame ([99a16af](https://github.com/playtron-os/cosmic-comp/commit/99a16afd08e326f013323e7bcce8d01206a70f6d))
* make voice orb input color not as strong so it doesnt turn yellow ([ef261e3](https://github.com/playtron-os/cosmic-comp/commit/ef261e3cb83afde1abf89b8bb10657346d2a623f))
* small shader tweek to reduce yellow color ([a81c8f8](https://github.com/playtron-os/cosmic-comp/commit/a81c8f863c906b18752d45ecf39fd6d208124fc9))

# [1.16.0](https://github.com/playtron-os/cosmic-comp/compare/v1.15.4...v1.16.0) (2026-04-14)


### Bug Fixes

* fix not reevaluating keyboard focus on layer show ([a47c01d](https://github.com/playtron-os/cosmic-comp/commit/a47c01dc1db4d5b0af3eda4c669b58188a8f9ce7))
* hard code F18 to activate voice mde ([8d3e9a6](https://github.com/playtron-os/cosmic-comp/commit/8d3e9a6115545144285727b9dea943f112278e91))
* update voice orb colors ([c9c2c77](https://github.com/playtron-os/cosmic-comp/commit/c9c2c77979a387289c6b3e20e7f09e7c3365c5a7))


### Features

* support moving output agnostic surfaces to active output when they get shown ([6789fec](https://github.com/playtron-os/cosmic-comp/commit/6789fec3de888a8182e80094fa7f5480b8186419))

## [1.15.4](https://github.com/playtron-os/cosmic-comp/compare/v1.15.3...v1.15.4) (2026-04-12)


### Bug Fixes

* add fallback voice mode binding of F18 ([36cc1ea](https://github.com/playtron-os/cosmic-comp/commit/36cc1ea344a81180bfdee3b8f3764769c888c775))
* update voice orb colors ([86d59b1](https://github.com/playtron-os/cosmic-comp/commit/86d59b1cb4a401fca81c05e15e350c321c3ea6b5))

## [1.15.3](https://github.com/playtron-os/cosmic-comp/compare/v1.15.2...v1.15.3) (2026-04-11)


### Bug Fixes

* focus the voice orb receiver ([0d44126](https://github.com/playtron-os/cosmic-comp/commit/0d4412673cfc6b122a83738e439bfbb00415b87e))
* use proper animated pipeline for animated resize protocol ([f332d64](https://github.com/playtron-os/cosmic-comp/commit/f332d640e9c4efb3ae076023229c656350effd66))

## [1.15.2](https://github.com/playtron-os/cosmic-comp/compare/v1.15.1...v1.15.2) (2026-04-10)


### Bug Fixes

* adjust voice orb attached mode scale ([2f12ef3](https://github.com/playtron-os/cosmic-comp/commit/2f12ef376c66ed3830f55eb352e0eb63ad30a5b5))

## [1.15.1](https://github.com/playtron-os/cosmic-comp/compare/v1.15.0...v1.15.1) (2026-04-09)


### Bug Fixes

* match F18 for voice mode as well ([f3585bf](https://github.com/playtron-os/cosmic-comp/commit/f3585bf341cbf586b087076efc097c8a677d5c7d))

# [1.15.0](https://github.com/playtron-os/cosmic-comp/compare/v1.14.0...v1.15.0) (2026-04-08)


### Features

* implement better animation driven pipeline to maximize/unmaximize windows ([6aa4b91](https://github.com/playtron-os/cosmic-comp/commit/6aa4b9179445527dd819f17027bbbab7a895bfa5))

# [1.14.0](https://github.com/playtron-os/cosmic-comp/compare/v1.13.0...v1.14.0) (2026-04-01)


### Features

* add support for COSMIC_ACTIVATION_TRUSTED_APPS for notifications app support ([b90b5a0](https://github.com/playtron-os/cosmic-comp/commit/b90b5a084bdff60816717c401048f57d6296428b))

# [1.13.0](https://github.com/playtron-os/cosmic-comp/compare/v1.12.0...v1.13.0) (2026-04-01)


### Bug Fixes

* update voice orb colors ([c8997f8](https://github.com/playtron-os/cosmic-comp/commit/c8997f897d74fb151e8be7f746a149a719f028ee))


### Features

* improve voice orb shader with processing state ([199379f](https://github.com/playtron-os/cosmic-comp/commit/199379fb551b9972c7e8cec0490435e3281841bb))

# [1.12.0](https://github.com/playtron-os/cosmic-comp/compare/v1.11.1...v1.12.0) (2026-03-31)


### Bug Fixes

* correctly update smithay version ([eea0fb7](https://github.com/playtron-os/cosmic-comp/commit/eea0fb7ad15e7877640a922c77266bf87228736d))
* error handling for dmabuf leak or other failures ([718d78a](https://github.com/playtron-os/cosmic-comp/commit/718d78ae1b583198197d2f7533abed79b80675aa))
* try color format abgr8888 before hdr formats, and add some more default handling to connection interface for panels ([cd381b7](https://github.com/playtron-os/cosmic-comp/commit/cd381b78c02a38c944356cb0ecaeedaa425cb1c7))
* update smithay to fix text/icon pixelation issue due to texture.frag mediump float ([2a7e3af](https://github.com/playtron-os/cosmic-comp/commit/2a7e3afd615d331e594a9a2359ed60f8f375dc59))


### Features

* properly handle X11 transient child windows (e.g. Android emulator side panel) ([d2ff036](https://github.com/playtron-os/cosmic-comp/commit/d2ff036a0db40b035d3fed667e1416575580b3ab))

## [1.11.1](https://github.com/playtron-os/cosmic-comp/compare/v1.11.0...v1.11.1) (2026-03-27)


### Bug Fixes

* remove local deps from cargo ([468896a](https://github.com/playtron-os/cosmic-comp/commit/468896a07c53ca8e9ad001828b61479a44830b08))
* unpin libcosmic versions ([ec13b0b](https://github.com/playtron-os/cosmic-comp/commit/ec13b0bc8ccbfc10116cacbd09262c7c1dade10d))

# [1.11.0](https://github.com/playtron-os/cosmic-comp/compare/v1.10.0...v1.11.0) (2026-03-26)


### Bug Fixes

* allow layer shell surfaces to fade out ([5045562](https://github.com/playtron-os/cosmic-comp/commit/50455623500579e874e0f73960e640a7c7804c48))
* disable blur shader border for layer surfaces with client corner radius ([e09cd9b](https://github.com/playtron-os/cosmic-comp/commit/e09cd9b803457bbf8123e815f15616e2013a1b53))
* reduce fade out duration for surface layer ([d6dc8f8](https://github.com/playtron-os/cosmic-comp/commit/d6dc8f866b78d7b8c8bd9b909e012300f3da5860))


### Features

* update voice orb and fix animation issues with voice input, and read audio level via shared memory buffer ([167f460](https://github.com/playtron-os/cosmic-comp/commit/167f4606b047e041a0a9088fc6702d35af414a2c))

# [1.10.0](https://github.com/playtron-os/cosmic-comp/compare/v1.9.0...v1.10.0) (2026-03-19)


### Features

* implement night shift setting ([4f7e632](https://github.com/playtron-os/cosmic-comp/commit/4f7e632a4c368c0faf5f06ac5f31176e8c6d2847))

# [1.9.0](https://github.com/playtron-os/cosmic-comp/compare/v1.8.2...v1.9.0) (2026-03-18)


### Bug Fixes

* cap window sizes properly so it fits within display bounds on first render ([c55743c](https://github.com/playtron-os/cosmic-comp/commit/c55743cb4687d267ad7db091bda36610fd44f828))
* fix window fade in animation playing when you drop them after drag ([59c67e8](https://github.com/playtron-os/cosmic-comp/commit/59c67e88cdb9fa2ef96555c4eec754d4b0023b24))
* update libcosmic to support proper font and letter spacing in SSD ([d2a2bd5](https://github.com/playtron-os/cosmic-comp/commit/d2a2bd5ecaa8e7ef73751aadc3bc4267ded4a517))
* use proper corner radius for windows ([abf2b1e](https://github.com/playtron-os/cosmic-comp/commit/abf2b1e2e5b562a09c52ba712cbcc0e8b72ab88a))


### Features

* fade in all floating windows when they appear ([e4b6008](https://github.com/playtron-os/cosmic-comp/commit/e4b6008f9e59b7e76d87bcd64cab4b67ec9fb492))

## [1.8.2](https://github.com/playtron-os/cosmic-comp/compare/v1.8.1...v1.8.2) (2026-03-14)


### Bug Fixes

* do not destroy blur on release request ([406164c](https://github.com/playtron-os/cosmic-comp/commit/406164c4572989f1f2dcfae1d54bad7e23a45e64))

## [1.8.1](https://github.com/playtron-os/cosmic-comp/compare/v1.8.0...v1.8.1) (2026-03-13)


### Bug Fixes

* animated resize not working on non active outputs ([30d2e46](https://github.com/playtron-os/cosmic-comp/commit/30d2e46552c221a328add782e66ff35a77bf4537))
* prevent too many reconfigure events from killing client wayland connection ([2ce0f7d](https://github.com/playtron-os/cosmic-comp/commit/2ce0f7d5fe8595ddfba3c95fc427db62f8347c4b))
* remove blurred windows borders and run fmt ([7e8a842](https://github.com/playtron-os/cosmic-comp/commit/7e8a8422ac51dcac325c5a96101af952456364b7))
* throttle reconfigure events during animated resize ([e5fbda8](https://github.com/playtron-os/cosmic-comp/commit/e5fbda850ad8dab247775d21db74a5317f64a0b4))

# [1.8.0](https://github.com/playtron-os/cosmic-comp/compare/v1.7.0...v1.8.0) (2026-03-10)


### Features

* implement custom tooltip protocol ([cc0ad50](https://github.com/playtron-os/cosmic-comp/commit/cc0ad50f0edf476896abc9ffced06633eb260fb4))

# [1.7.0](https://github.com/playtron-os/cosmic-comp/compare/v1.6.0...v1.7.0) (2026-03-05)


### Features

* show hidden surfaces if there are no windows left on that display ([1cca745](https://github.com/playtron-os/cosmic-comp/commit/1cca74596c4a03f866dc9309c83bcdb56b524e1d))
* update smithay ([883dc73](https://github.com/playtron-os/cosmic-comp/commit/883dc73add460eb65ea6f8ba4ea596bb3d032ac3))

# [1.6.0](https://github.com/playtron-os/cosmic-comp/compare/v1.5.0...v1.6.0) (2026-02-27)


### Bug Fixes

* adjust SSD header height ([4f8c9d1](https://github.com/playtron-os/cosmic-comp/commit/4f8c9d150b55bd036cbd6ff910c4fc35bdc926fc))
* handle error logging for wayland disconnect ([2f89004](https://github.com/playtron-os/cosmic-comp/commit/2f8900480327dba1705bc2a3b0fbce4559d1418e))
* update height of SSD header ([083788f](https://github.com/playtron-os/cosmic-comp/commit/083788f51d789e1fc52946aa737d35306e044354))


### Features

* perf improvements for blur and gpu detection for qualcom socs ([3ba519f](https://github.com/playtron-os/cosmic-comp/commit/3ba519f7c1ef98da1b48c49be695efc99041f97d))

# [1.5.0](https://github.com/playtron-os/cosmic-comp/compare/v1.4.0...v1.5.0) (2026-02-20)


### Bug Fixes

* correctly move embedded children to same output as parent and reconfigure size on start ([f32fd1c](https://github.com/playtron-os/cosmic-comp/commit/f32fd1c7833ba1ffb0ea10cafe35d0c0601c7c4c))
* do not allow voice mode while session is locked ([0c43581](https://github.com/playtron-os/cosmic-comp/commit/0c435814379ecb361dd6597d5a40df48edf5ae35))
* dont skip 1x1 pixel surfaces ([3d4da11](https://github.com/playtron-os/cosmic-comp/commit/3d4da1168f0896aea35a57232fb8dfd7051153a2))
* prevent window resize past area bounds ([b188595](https://github.com/playtron-os/cosmic-comp/commit/b188595e09fd3dddeb2374fae05025e0f5606379))
* prevent windows from being dropped behind exclusive zone surfaces ([d39bf46](https://github.com/playtron-os/cosmic-comp/commit/d39bf463f40df146da26cef6f7547f8849dc957a))
* shadows not rendering for unmazimied windows ([100764b](https://github.com/playtron-os/cosmic-comp/commit/100764ba5176b0045ccce7728961b8c65b012f3f))


### Features

* add mode to layer auto hide protocol and implement always hidden mode ([65d957e](https://github.com/playtron-os/cosmic-comp/commit/65d957eda7065c511b50afd5be6fb3862f72ac83))
* implement backdrop color protocol ([c856a52](https://github.com/playtron-os/cosmic-comp/commit/c856a52348766fb81cb55c9c299bf9b6ffb42959))
* implement layer auto hide protocol ([e493166](https://github.com/playtron-os/cosmic-comp/commit/e493166e61bc0243400efb0697372925f62e7260))
* improve header icons by pre downscaling them with Lanczos3 ([7479359](https://github.com/playtron-os/cosmic-comp/commit/74793598bb862ec561e4d25a94867755de01cf0b))
* SSD header bar with custom styling, cursor handling, icon caching, and corner radius ([039adc8](https://github.com/playtron-os/cosmic-comp/commit/039adc8feeb478e6b41282fb01cb19dfae4c24d4))
* update libcosmic ([9de6b14](https://github.com/playtron-os/cosmic-comp/commit/9de6b143e5d7232c88cda800dde2312ce3e377ed))
* update voice orb shader colors ([1f3bd9f](https://github.com/playtron-os/cosmic-comp/commit/1f3bd9f62ac0f36b9c2c7d0ba48148566c25d71a))

# [1.4.0](https://github.com/playtron-os/cosmic-comp/compare/v1.3.0...v1.4.0) (2026-02-07)


### Features

* add an additional backdrop on top of blur for extra effect ([89e52fc](https://github.com/playtron-os/cosmic-comp/commit/89e52fce7e5547341101609465b5b5d2986db72b))
* support fallback for blur effect in software rendering ([b5f10b1](https://github.com/playtron-os/cosmic-comp/commit/b5f10b186178fab058db7d464a57215550fa5ee6))

# [1.3.0](https://github.com/playtron-os/cosmic-comp/compare/v1.2.0...v1.3.0) (2026-02-06)


### Bug Fixes

* patch smithay packages for make run-debug to work properly ([b080bfa](https://github.com/playtron-os/cosmic-comp/commit/b080bfa060602a5347d536a4b1ca49befa2e8592))
* super key pressing interaction with home mode ([72b25d4](https://github.com/playtron-os/cosmic-comp/commit/72b25d478627caf7a862b6699fdeceebebee3e32))


### Features

* add option to activate voice mode via button from client ([4d15dae](https://github.com/playtron-os/cosmic-comp/commit/4d15daec42e08221a73eabdec62714e4ac90c006))
* update blur window white opacity to 0.9% ([9722159](https://github.com/playtron-os/cosmic-comp/commit/9722159203379c83a832385ac399764f2cd612b1))

# [1.2.0](https://github.com/playtron-os/cosmic-comp/compare/v1.1.0...v1.2.0) (2026-02-04)


### Features

* implement force_close in top level protocol ([ebccfdf](https://github.com/playtron-os/cosmic-comp/commit/ebccfdf89d61ced9e836d891b0a55dbc5bdc2df8))

# [1.1.0](https://github.com/playtron-os/cosmic-comp/compare/v1.0.1...v1.1.0) (2026-02-04)


### Features

* add layer_surface_dismiss protocol ([25e87fb](https://github.com/playtron-os/cosmic-comp/commit/25e87fbcc867254c989b6b723cb5ab59751514be))

## [1.0.1](https://github.com/playtron-os/cosmic-comp/compare/v1.0.0...v1.0.1) (2026-02-04)


### Bug Fixes

* add missing release CI permission for PR/issues ([d464435](https://github.com/playtron-os/cosmic-comp/commit/d4644353a11d2ae31736fbdea31b9a43f0c2f8b2))

# 1.0.0 (2026-02-04)


### Bug Fixes

* `cargo update` to target available cosmic-text commit ([f7b062a](https://github.com/playtron-os/cosmic-comp/commit/f7b062aa81aacf76cf4dd90154a88df8462e4337))
* activate voice orb in maximized window if there is no focus ([79acc35](https://github.com/playtron-os/cosmic-comp/commit/79acc35d46196d064a111a16c7c573909a3bca5a))
* activation of an element outside the current workspace ([2c01c94](https://github.com/playtron-os/cosmic-comp/commit/2c01c944770a9ac085166f4ae1b3ad14e0f8b001))
* Add `Element::kind` method to `CosmicElement` ([916b772](https://github.com/playtron-os/cosmic-comp/commit/916b7729a542c2f037522bc03efc7d2980faa939))
* add cosmic-comp-config to nix flake ([e37cfbf](https://github.com/playtron-os/cosmic-comp/commit/e37cfbf5956e7ae2de42122e01a064c430974f3b))
* Add data_control to config file ([a0aa8fb](https://github.com/playtron-os/cosmic-comp/commit/a0aa8fb8b22f387eafd9da46e9109146b6b70442))
* add missing libdisplay-info library into flake.nix ([7425ffb](https://github.com/playtron-os/cosmic-comp/commit/7425ffbad9808f024777540657af94c780b64714))
* Add second touchpad hotkey config for Pangolin keyboards ([25904ca](https://github.com/playtron-os/cosmic-comp/commit/25904ca1890bd5082aa5b4996413f5031e7bc315))
* add wayland, x11 libraries in runtime ([55c83a3](https://github.com/playtron-os/cosmic-comp/commit/55c83a3e308c073c288f7491decf9b9f94e57ef6))
* always exclude top/overlay surfaces from blur textures ([8ad3452](https://github.com/playtron-os/cosmic-comp/commit/8ad3452f0f3f15db2b5094a44a1237abc64eb0c3))
* Apply `scroll_factor` to v120 events ([36bf611](https://github.com/playtron-os/cosmic-comp/commit/36bf611bac75bdf4fdff6e0eda673367745c7bd7))
* apply activated state when updating maximized layout ([13e67f3](https://github.com/playtron-os/cosmic-comp/commit/13e67f3dc2bd14d80de0d200671be70f9fbdc761))
* apply transform to damage before blitting ([8d5541b](https://github.com/playtron-os/cosmic-comp/commit/8d5541b27f783a06cc51448b552432291b5e8cce))
* Apply window snapping properly on multiple-output configurations ([#1529](https://github.com/playtron-os/cosmic-comp/issues/1529)) ([7f814a4](https://github.com/playtron-os/cosmic-comp/commit/7f814a445c40234211fc194d1f9ac9341f823d76))
* Avoid crash on malformed keymap config ([#361](https://github.com/playtron-os/cosmic-comp/issues/361)) ([e625a22](https://github.com/playtron-os/cosmic-comp/commit/e625a2278307968fafe8fe36e118800c10f6f696))
* blurred backdrop shader causing corner artifacts ([51640b0](https://github.com/playtron-os/cosmic-comp/commit/51640b0465f0e485f9b39e41929489c743bf2405))
* border for embedded windows ([48d03e3](https://github.com/playtron-os/cosmic-comp/commit/48d03e39434ff444aead6441cde10ab762577b5c))
* calculate correct pointer under coordinates for embedded windows ([4ec80e8](https://github.com/playtron-os/cosmic-comp/commit/4ec80e885b676dc000f6a05e0440f17e030cf2a9))
* change animated resize to properly calculate x y coords and not move y coords at all ([89f1cef](https://github.com/playtron-os/cosmic-comp/commit/89f1cef8e3d58d5913a8505606dedf09bd9c823d))
* check grabbed window in addition to focused window when activating voice orb ([ac77e98](https://github.com/playtron-os/cosmic-comp/commit/ac77e98d6eb477d6a40fbe97300cd36b793a8bb7))
* check layer map for root popup surface ([e1aa8f7](https://github.com/playtron-os/cosmic-comp/commit/e1aa8f7cb46f35d99e37cdb19058de8a3b4f4659))
* clamp mouse position just below output size ([929f4fc](https://github.com/playtron-os/cosmic-comp/commit/929f4fcb054710ee73d781f7050f0a005981fa0e))
* clear some states on voice orb animation properly ([f90bfe9](https://github.com/playtron-os/cosmic-comp/commit/f90bfe92a178d8389dd41c535fe42444f9229d74))
* **config:** move cosmic-randr-shell to main branch ([64a9c1b](https://github.com/playtron-os/cosmic-comp/commit/64a9c1badfd6f9b3af3af6db2b07b3d76d6f5102))
* **config:** PlayPause is not a valid xkb keysym ([3dd3460](https://github.com/playtron-os/cosmic-comp/commit/3dd3460ee0caef370b37c33913791f9f0cba12ab))
* **config:** wrong config context for system actions on startup ([476470e](https://github.com/playtron-os/cosmic-comp/commit/476470e6f1d61e0005179661bccedb9c3e884860))
* **corner-radius:** error handling and cleanup ([228af10](https://github.com/playtron-os/cosmic-comp/commit/228af1037ad7677100f878a2db41da9c8804a705))
* **corner-radius:** force redraw after corner radius change ([4e30513](https://github.com/playtron-os/cosmic-comp/commit/4e305136737c128be2ec27111b8a53e85691961c))
* **corner-radius:** guard against corner radius being too large ([2d9d83d](https://github.com/playtron-os/cosmic-comp/commit/2d9d83d3bdf947f3b6e6bb5a676de700ccf9179f))
* **corner-radius:** post protocol errors ([819887e](https://github.com/playtron-os/cosmic-comp/commit/819887e298602371ab05b5f6206d7ff531bd33be))
* **corner-radius:** properly handle no value, and use geometry ([2873d6b](https://github.com/playtron-os/cosmic-comp/commit/2873d6b27e3435a2c74b65e89b3213a889732d7f))
* **corner-radius:** use cached state ([242e465](https://github.com/playtron-os/cosmic-comp/commit/242e465d423525cb844fcf73b723864c23b5a630))
* cosmic-protocols rev ([b793780](https://github.com/playtron-os/cosmic-comp/commit/b7937807adbb964a09e914bea3e96026e6c4f4e1))
* dead lock during grabbed orb state read ([47cc3d3](https://github.com/playtron-os/cosmic-comp/commit/47cc3d3d2510201504e8cfea9d663e363e86d682))
* dependency ([71ac7cc](https://github.com/playtron-os/cosmic-comp/commit/71ac7cc86ca8fe08078053465ee923c2437728d2))
* display blur effect during window drag ([9bc713d](https://github.com/playtron-os/cosmic-comp/commit/9bc713de0fb10f0ec7c8f52b45e7474b244875a9))
* don't assume previous workspace still exists [#1588](https://github.com/playtron-os/cosmic-comp/issues/1588) ([c9e64ac](https://github.com/playtron-os/cosmic-comp/commit/c9e64acad2860e1068cf11468da00acc7ee2f86d))
* embedded view popup positioning and output mapping ([a6f7f5c](https://github.com/playtron-os/cosmic-comp/commit/a6f7f5c89f2da8cc64efcd5ee6667b2d2010a8d6))
* embedded window handling for separate outputs ([559c152](https://github.com/playtron-os/cosmic-comp/commit/559c15206d67f62be81c0520ea2bd762090c672c))
* enable voice orb to appear on any display ([1d3e21c](https://github.com/playtron-os/cosmic-comp/commit/1d3e21c9ae440c637a5575becc86998ebf9ce390))
* exclude home layer surfaces from window blur effects ([e50b5a5](https://github.com/playtron-os/cosmic-comp/commit/e50b5a5fde57281317e5d613ead3c1ce0efcbffa))
* fade out top layer shell surfaces in voice mode as well ([21b4de6](https://github.com/playtron-os/cosmic-comp/commit/21b4de6f35edc429fe07d2403d2e26c6b603f901))
* filter by active workspace in overlap notify ([76863aa](https://github.com/playtron-os/cosmic-comp/commit/76863aaf9b07e5dc62c2772c0d8e7354a58234a4))
* handle VT switch only in KMS backend to avoid crashes ([1bf8482](https://github.com/playtron-os/cosmic-comp/commit/1bf848225e056fb73162bed78cfeee512a206ad4))
* hide top level layers during voice orb mode except panel ([9bb710c](https://github.com/playtron-os/cosmic-comp/commit/9bb710cae71883abcc5c75484247342ff532c8a8))
* **iced:** use internal_ref size ([892c05f](https://github.com/playtron-os/cosmic-comp/commit/892c05f4fca31af5cbe6960ffbd20051d9032096))
* ignore resize icon change from embedded windows ([60928d1](https://github.com/playtron-os/cosmic-comp/commit/60928d102eb71c51f1f53eb634042c6bd23e75f1))
* improve damage tracking ([3132767](https://github.com/playtron-os/cosmic-comp/commit/3132767a6014d11da4c4bc0a408bedf01d3f77a2))
* in CI upload rpms and add concurrency config ([210cc5c](https://github.com/playtron-os/cosmic-comp/commit/210cc5c39bda6dabfa9ee24ab4ac9a689a911206))
* **init_logger:** Directive::from_str expects warn ([cffaf9e](https://github.com/playtron-os/cosmic-comp/commit/cffaf9ee301fd710229b8d6d1cefe4f4fb6c1e05))
* **input:** pointer clamping ([9e143da](https://github.com/playtron-os/cosmic-comp/commit/9e143da8145d0c610d0569c178551d3f0a2b84a4))
* **input:** zombie process from Action::Spawn ([9d0e1e8](https://github.com/playtron-os/cosmic-comp/commit/9d0e1e88cea6bf9cbb59dd508d91e19014635ffa))
* intercept modifiere press for shortcut with no key ([4f04313](https://github.com/playtron-os/cosmic-comp/commit/4f043133bc0365283fbb7aafd81a80dc3e2cecee))
* issues with blur/focus z order of overlapping windows with embeds ([fbca3dc](https://github.com/playtron-os/cosmic-comp/commit/fbca3dcaeff4a363fdd0df584303c316317fa1d2))
* **kms:** early exit when iterating over crts ([0a8da05](https://github.com/playtron-os/cosmic-comp/commit/0a8da05847290439951561fd3ebae581a0e0ff5b))
* manually scale the damage ([5b89ad2](https://github.com/playtron-os/cosmic-comp/commit/5b89ad27fb56dcbd926ef53bd08313255ba8e449))
* **menu:** styling ([b7a34bd](https://github.com/playtron-os/cosmic-comp/commit/b7a34bdd1ee4e0f7570041fc87bf4da115e40654))
* **menu:** use corner radius from theme ([50d6dc3](https://github.com/playtron-os/cosmic-comp/commit/50d6dc3d21c5c373d81f294d9b869a76205840a2))
* merge conflict from voice orb changes with revert of corner radius theme changes ([a3e3517](https://github.com/playtron-os/cosmic-comp/commit/a3e3517af251450f39c18897711714460fb7f86d))
* minor improvement to window minimizing animation ([4d65050](https://github.com/playtron-os/cosmic-comp/commit/4d650503dbab097b6f2f87c95da1a9d6bf021f93))
* only activate the active window of the stack on activation ([f2d6f70](https://github.com/playtron-os/cosmic-comp/commit/f2d6f70e13f6e7510326ced59ae5db6df705d955))
* only move the grabbed window if a new zone is chosen ([939ac1f](https://github.com/playtron-os/cosmic-comp/commit/939ac1f61b4ec514c7f66e533aeeb3fa7db5576b))
* only require modifiers for voice input if they are defined as true ([c4dd49f](https://github.com/playtron-os/cosmic-comp/commit/c4dd49f0909b874fe841d0a95d4cf3ee7371f2b5))
* only suppress keys when an action is on press ([f406bf3](https://github.com/playtron-os/cosmic-comp/commit/f406bf33ae61867304a892410758bff5132250bc))
* **outline:** pass the radii in the correct order for the shader ([e476153](https://github.com/playtron-os/cosmic-comp/commit/e476153086bf81c1550eaf520b2df1bd50430b24))
* **overlap:** check if window is sticky ([ef5a1a3](https://github.com/playtron-os/cosmic-comp/commit/ef5a1a3284f4dd695f9a003f8fa8e305a3d84562))
* pointer leave not triggering on embedded windows ([fe5cf8a](https://github.com/playtron-os/cosmic-comp/commit/fe5cf8a52b55b2fff3e0f8a9c372eb19785f3bd2))
* **popup:** avoid panic when None ([04d3e1e](https://github.com/playtron-os/cosmic-comp/commit/04d3e1ed590feb774d08968d0e90930f081b40b5))
* prevent blur on blur texture artifacts ([9af1307](https://github.com/playtron-os/cosmic-comp/commit/9af13070a993359b5dbfa47bfc393806a54bdcff))
* prevent window fade out animation if quickly pressing voice orb shortcut ([38a5f18](https://github.com/playtron-os/cosmic-comp/commit/38a5f18a55eef1881f1949c4827b992adaa77fe3))
* properly fade in surfaces when dismissing voice orb ([620f5b4](https://github.com/playtron-os/cosmic-comp/commit/620f5b48fa5c58f4b2c29035c608b0aa53af3e95))
* properly focus default surface when pressing super key ([58831d7](https://github.com/playtron-os/cosmic-comp/commit/58831d744f9ac20ae3dabe64ba36075c14bf729b))
* properly force configur efor embedded windows post mapping ([2a2ee3d](https://github.com/playtron-os/cosmic-comp/commit/2a2ee3de8f07f2152aa7233c745d113bf31cd509))
* properly hide only correct surfaces during voice mode ([c04e524](https://github.com/playtron-os/cosmic-comp/commit/c04e5241450f3e05828a76a7a973d341c5537a34))
* properly remove elements from the focus_stack ([714e803](https://github.com/playtron-os/cosmic-comp/commit/714e80366d31edf0c5fd0e8231f6f97e84fad929))
* properly resize voice orb when window size changes ([7c1b6f1](https://github.com/playtron-os/cosmic-comp/commit/7c1b6f13b77b6c1e31aae4af9d6c6bc969f9660c))
* **protocol/workspace:** Handle cosmic-workspace-v2 destroy requests ([8b63e09](https://github.com/playtron-os/cosmic-comp/commit/8b63e09ddcb0120986afe89c7db29887a72fe243))
* reduce size of top snap range ([f859110](https://github.com/playtron-os/cosmic-comp/commit/f85911020a93b051227e44f86b5e5c6e0ba2fdec))
* Remove extra scaling of borders ([ad2cba3](https://github.com/playtron-os/cosmic-comp/commit/ad2cba324e81797f7ba7d83aa848a85ac1e143d1))
* remove non-existent input 'nixpkgs' from 'crane' ([7c24e36](https://github.com/playtron-os/cosmic-comp/commit/7c24e361aace1e3949d8f94ee02bb1c557549f63))
* reset name and state for workspaces after moving them to a new group ([048490d](https://github.com/playtron-os/cosmic-comp/commit/048490d57ea0a16332a4e82b5d627c38653d44ea))
* return if output is none ([62bf4b1](https://github.com/playtron-os/cosmic-comp/commit/62bf4b1ccd3826b7e729e01748d82de92489f6eb))
* send pending activated instead of current activated to toplevel_info ([895ea6a](https://github.com/playtron-os/cosmic-comp/commit/895ea6aec11485b8f142a41b2c05618c571edd69))
* set autotile for al workspace sets when there is a change ([d1aac38](https://github.com/playtron-os/cosmic-comp/commit/d1aac380fff5447a14fa1fc1023ea1078c0226a4))
* set the workspace of the activated token's surface to urgent if not focusing it ([495d772](https://github.com/playtron-os/cosmic-comp/commit/495d772a38d1f22ec6d3960c4c1103d6678c4009))
* **shell:** distinguish between unmapping and destroying surfaces ([15b6b67](https://github.com/playtron-os/cosmic-comp/commit/15b6b678c1303b6ace3af47d21abb2df1d583824)), closes [#1816](https://github.com/playtron-os/cosmic-comp/issues/1816)
* **shell:** lagging tab animations ([c16b86d](https://github.com/playtron-os/cosmic-comp/commit/c16b86d1bf57ee3a922bdb480198f5e5cc61fcdb))
* show voice orb during attached window drag ([8bb0f1c](https://github.com/playtron-os/cosmic-comp/commit/8bb0f1ca3709c72c078e803b09d5519cc9734e87))
* skip including dragged windows in blur captures ([38984b0](https://github.com/playtron-os/cosmic-comp/commit/38984b01cd279181f3d63bda48f6d290aca06710))
* **stack:** get gradient colors from theme ([26400b5](https://github.com/playtron-os/cosmic-comp/commit/26400b5fcd9e628169a770b3e33180b573de19cd))
* **stack:** set correct colors for light theme ([d9750ff](https://github.com/playtron-os/cosmic-comp/commit/d9750ffb768e74360a109fb8cc8b698af73d8dc1))
* **stack:** set linear gradient to 90 degrees ([feaf572](https://github.com/playtron-os/cosmic-comp/commit/feaf57225a5d35656adca6ccb827ea8eec89c8c1))
* **stack:** show tab text overlay when overflowing ([e7cf858](https://github.com/playtron-os/cosmic-comp/commit/e7cf8581ccde29843dfd57370895b8b69598442c))
* **stacks:** panic in layout when tabs are paginated ([134bb9f](https://github.com/playtron-os/cosmic-comp/commit/134bb9f59b7c7c9ad2ec6e28a3c6ca0f1c151244))
* Super+Escape locks screen per Pop!_OS convention ([f366a5d](https://github.com/playtron-os/cosmic-comp/commit/f366a5dc87152c698c4327ec81a630847128cd28))
* support per-corner radius ([c6320ee](https://github.com/playtron-os/cosmic-comp/commit/c6320eec0c4cd37c32516ff19c033b8ad1912f68))
* Toplevel disappearing after unmap ([ea429a7](https://github.com/playtron-os/cosmic-comp/commit/ea429a778e7b2de6e73342dc66b0d6626bb963d1))
* unresolved import [of std] on NixOS in RA ([6fc7fd2](https://github.com/playtron-os/cosmic-comp/commit/6fc7fd2bafdf211c0f28b2438155bb06b4200d73))
* unset grab if focusing exclusive layer shell surface ([e55d16b](https://github.com/playtron-os/cosmic-comp/commit/e55d16b87bc00d24b03a0d3924e04125f0ae6be1))
* update maximized elements when refreshing the floating layout ([a3df48f](https://github.com/playtron-os/cosmic-comp/commit/a3df48fe176e3fdabcc3eb8d54e21fc9aa2fe3d5))
* use --locked instead of --frozen ([f1e3e3b](https://github.com/playtron-os/cosmic-comp/commit/f1e3e3bf984d72b7a72004e1d044a76600990f7a))
* use `cosmic-term` and `cosmic-screenshot` by default ([2834551](https://github.com/playtron-os/cosmic-comp/commit/2834551ffb3c5e9cbf9885db915a2ff359a19c01))
* voice orb follow attached window's position ([60f36d5](https://github.com/playtron-os/cosmic-comp/commit/60f36d54cb4b39772c358e12c680ce55b5fc29ff))
* **window:** center window title ([ea2215e](https://github.com/playtron-os/cosmic-comp/commit/ea2215ec3f5ae6c00fe723746f43179e06143d38))
* windows are clipped in overview mode causing visual artifact ([509abd0](https://github.com/playtron-os/cosmic-comp/commit/509abd0d6441b21fa8c980fdf3e8a72a49e8564a))
* windows being included in blur texture for bottom layer surfaces ([5eb387b](https://github.com/playtron-os/cosmic-comp/commit/5eb387b924420b2d6f9c996af4380f388335fddd))
* **zoom-ui:** Persist accessibility_zoom increment config value. ([8a5d78d](https://github.com/playtron-os/cosmic-comp/commit/8a5d78dbb0eef579bc6afab5315363e872a15185))


### Features

* ad dismiss protocol call for voice input orb ([e46e6f0](https://github.com/playtron-os/cosmic-comp/commit/e46e6f0ec219432db12c5d873f88a0701e981720))
* add alt-tab shortcut ([ea6b45a](https://github.com/playtron-os/cosmic-comp/commit/ea6b45a5449d1c33c7deb7c61bb519d161d2d89d))
* add in-between empty state in voice orb animation in/out ([d0912dd](https://github.com/playtron-os/cosmic-comp/commit/d0912dd318e9284e9de6a5fe3e60a163afe59619))
* add layer surface visibility protocol implementation ([f86e064](https://github.com/playtron-os/cosmic-comp/commit/f86e06415a21d932607e02330786a36c4fbe6fd3))
* add shortcut for input source switch ([b93db87](https://github.com/playtron-os/cosmic-comp/commit/b93db878e2f3613f68a39fee30add9f7abbf7ca6))
* add shortcut for super-tab ([8df49bb](https://github.com/playtron-os/cosmic-comp/commit/8df49bb7c1efca4b359579281c20b513d079bd0f))
* add tiling variables to cosmic config ([5eb5af4](https://github.com/playtron-os/cosmic-comp/commit/5eb5af46756f2f2bc5e812cf68fe4c4b79efd924))
* Allow fractional xwayland client scale ([7472351](https://github.com/playtron-os/cosmic-comp/commit/7472351de03ef02ec26ad05a9c73d7fb27760d1d))
* alt+shift+tab ([5f650e7](https://github.com/playtron-os/cosmic-comp/commit/5f650e7d088bc5f56584c3a78612a88b5a4dfc49))
* bump version to 1.0.4 ([21300f5](https://github.com/playtron-os/cosmic-comp/commit/21300f5f6a97b9a071eed652ae035ffd61f7f72f))
* check child PID for surface embed protocol to enforce security ([bf38dfa](https://github.com/playtron-os/cosmic-comp/commit/bf38dfaf6d1f7164f675cba57182d93589a25e14))
* **config:** load cosmic-randr output Lists ([bb8e066](https://github.com/playtron-os/cosmic-comp/commit/bb8e066d6d4013e7ac559739f2b570e689c615e1))
* configurable keyboard repeat rate and delay ([dcc4873](https://github.com/playtron-os/cosmic-comp/commit/dcc4873e6059a28078298bdc7a54912df89f1f25))
* corner radius for CosmicMapped ([3465ce7](https://github.com/playtron-os/cosmic-comp/commit/3465ce7602c7e2a116617c28b1fbfb45762402a2))
* corner-radius protocol support ([b3aa104](https://github.com/playtron-os/cosmic-comp/commit/b3aa10436a70d88683a2d762edfbd016decc1546))
* custom protocols and window sizing animations ([f5599f9](https://github.com/playtron-os/cosmic-comp/commit/f5599f96f16bf37d1dfb4d3f3e02e99e19c973e2))
* decrease blur iterations for performance ([849714c](https://github.com/playtron-os/cosmic-comp/commit/849714ce82884260939254506970c056e77f7075))
* dont show layer shell surfaces until voice burst anim finishes ([75ee876](https://github.com/playtron-os/cosmic-comp/commit/75ee876f3e29580dca75aed70ff70d65cdf41e21))
* fade in windows that open maximized ([5d95f10](https://github.com/playtron-os/cosmic-comp/commit/5d95f10682927b1c519b19cd83153f29e3e0cbed))
* floating window tiling gaps ([9022747](https://github.com/playtron-os/cosmic-comp/commit/90227471bf66b4f63554905becfcf3308f3fedeb))
* fully implement surface embed protocol by pid and refactor initial implementation ([f0f84c8](https://github.com/playtron-os/cosmic-comp/commit/f0f84c8d4e38ea97deacc95529ab1b9faaad12f3))
* handle voice orb attached mode animations ([774493e](https://github.com/playtron-os/cosmic-comp/commit/774493ee88479499f5f103a581bfee2d836b0288))
* implement ability to hide layer surfaces while in home mode ([90ee839](https://github.com/playtron-os/cosmic-comp/commit/90ee839b154c67275f5bd2904407f1fe7b79bb8a))
* implement better voice orb transition from floating to new chat ([92f46e3](https://github.com/playtron-os/cosmic-comp/commit/92f46e33262364a69c95fe55fb963502e0b79a7b))
* implement blur effect for floating windows ([c583135](https://github.com/playtron-os/cosmic-comp/commit/c5831351733b76da145b01b8786f97e4c8df4a70))
* implement blur effect for layershell surfaces ([9c4435b](https://github.com/playtron-os/cosmic-comp/commit/9c4435beb4fc8d652d2269780859c675f822d80e))
* implement blur throttle to improve performance ([cc8f9a7](https://github.com/playtron-os/cosmic-comp/commit/cc8f9a7cc63b5ac7ba90e5cc95f9663d5fe6057d))
* implement corner radius protocol for layer shell surfaces ([41ab5c4](https://github.com/playtron-os/cosmic-comp/commit/41ab5c4f24a14d29f03e95395fa47bb43b04ec70))
* implement exclusive mode protocol ([4810b76](https://github.com/playtron-os/cosmic-comp/commit/4810b765def268fa852d1c7952fe93498fb7e26f))
* implement home state and visibility protocol ([d945908](https://github.com/playtron-os/cosmic-comp/commit/d9459084b11191fd819c3c4d1fb0f2bb6b8c559f))
* implement layer surface shadows ([c5e6139](https://github.com/playtron-os/cosmic-comp/commit/c5e6139f4183c82610c45f6a7a8bd100e9191ba6))
* implement proper multi shadow support and integration with blur windows ([7d36fac](https://github.com/playtron-os/cosmic-comp/commit/7d36face8680df78c8a49354abf9694780b626c1))
* implement support for 2 shadow layers per window ([edbcace](https://github.com/playtron-os/cosmic-comp/commit/edbcace5b20ab5f37d7337724a88cc7f8b6769e8))
* improve blur effect to match figma blur ([37438e2](https://github.com/playtron-os/cosmic-comp/commit/37438e2926fdd79d39a5180472c65b9b46e3b77b))
* improve blurred backdrop shader border radius and fix issues with layer shell blur passes ([6b2ba02](https://github.com/playtron-os/cosmic-comp/commit/6b2ba0219933f2b297b834e27f514560076bf637))
* integrate audio level into voice mode protocol ([436c3d7](https://github.com/playtron-os/cosmic-comp/commit/436c3d71a5549124d3b45cb42d67513b6aa4f7d6))
* integrate focus_input event in voice mode protocol and change shortcut to be Super and Copilot keys ([68b2faf](https://github.com/playtron-os/cosmic-comp/commit/68b2faf10aaa3bacdc0442b41cabedbb62387da7))
* integrate voice mode with orb shader animation ([89ffa89](https://github.com/playtron-os/cosmic-comp/commit/89ffa8952946ed17abfd85e5cc222989c25fb651))
* maximize/half tiling drag zones ([a4f3006](https://github.com/playtron-os/cosmic-comp/commit/a4f300631348cc9c64aba9d069986519a7d3b3a8))
* power button handling ([8e3590f](https://github.com/playtron-os/cosmic-comp/commit/8e3590fb4df997d1bb44ad935cd02c06341b10cc))
* properly capture copilot key and super key for voice mode ([49473da](https://github.com/playtron-os/cosmic-comp/commit/49473da229833b6f7f713b372403752feddf57af))
* properly update voice orb based on voice input level ([e8a9b22](https://github.com/playtron-os/cosmic-comp/commit/e8a9b229d3634b0f9bb171ed814bd1fd567a5b19))
* runtime configurable keybindings ([553c49b](https://github.com/playtron-os/cosmic-comp/commit/553c49b42b5941002b3dd1e7b3fddaf58760dcd1))
* support forwarding right window keys to applications ([ebe0e38](https://github.com/playtron-os/cosmic-comp/commit/ebe0e38ec31dd4654a2ba965e2f7ef14341ce1b7))
* support moving parent windows with embedded windows to different monitors ([cd26ec6](https://github.com/playtron-os/cosmic-comp/commit/cd26ec646a84346074c2800de1f98eaf15136b51))
* sync state with greeter ([b3a67bc](https://github.com/playtron-os/cosmic-comp/commit/b3a67bca50138d504b707880be37495e2b133f4f))
* theme integration ([abbe94e](https://github.com/playtron-os/cosmic-comp/commit/abbe94e6e14efa165647b1a0b95ee3e5a98a972f))
* try to hide pending window embeds by surface id before embedding happens ([b1ec378](https://github.com/playtron-os/cosmic-comp/commit/b1ec378eda9fe4bcbd63eb5e11e9204fa06766b8))
* update smithay ([cc3a358](https://github.com/playtron-os/cosmic-comp/commit/cc3a3588ea55a15df3ee902dffc53586fbdc5f2b))
* update smithay back to original origin after fix upstreamed ([45b25aa](https://github.com/playtron-os/cosmic-comp/commit/45b25aa89edcbf2b768e962729cd5ad23eec7654))
* update smithay to fix panic during surface export with no data ([426b250](https://github.com/playtron-os/cosmic-comp/commit/426b25040de739fd4ff7ad2d9b9a54193e747589))
* update voice mode to have receivers per surface ([dbdd16f](https://github.com/playtron-os/cosmic-comp/commit/dbdd16f1ca3927ec8902cb786c10a25a4c080f65))
* use applet host to activate launcher and app library ([1a6d5f6](https://github.com/playtron-os/cosmic-comp/commit/1a6d5f644a9664d4b9d2f82dd47f41ee083abd97))
* use upstream smithay ([486bcd1](https://github.com/playtron-os/cosmic-comp/commit/486bcd13c6ce72d49a838e4b2c0622f4be7e054e))
* workspace switching touchpad gestures ([fc2173d](https://github.com/playtron-os/cosmic-comp/commit/fc2173d028e36d8bf23e50e6805d0fc55a89078a))


### Performance Improvements

* **stack:** reduce allocations in Tabs::new ([a6c26b0](https://github.com/playtron-os/cosmic-comp/commit/a6c26b0bde5b55a3431085a8d6967ef05fea50d6))


### Reverts

* Revert "deps: Update smithay" ([146a489](https://github.com/playtron-os/cosmic-comp/commit/146a4893ca973936a8978232a7b7f1df295da112))
* Revert "feat: power button handling" ([8a3436e](https://github.com/playtron-os/cosmic-comp/commit/8a3436edb295597b22278068c664cdb673b25a9b))
