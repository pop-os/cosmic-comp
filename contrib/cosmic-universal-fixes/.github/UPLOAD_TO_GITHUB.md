# Upload this fix to GitHub (to link in COSMIC issues)

## Why

`pop-os/cosmic-comp` needs a PR, but you need a fork first. This repo (`Default Project`) is already a git repo with the fix commit `29115e5`. Push it to your own GitHub so you can link `https://github.com/YOURUSER/cosmic-universal-fixes` in `pop-os/cosmic-comp#2336` and `pop-os/cosmic-epoch#1526`.

## Steps (Web UI, no gh needed)

1. Create empty repo on GitHub: https://github.com/new
   - Name: `cosmic-universal-fixes` (or `cosmic-rust-fixes`)
   - Public, no README, no .gitignore
   - Create

2. Push this local repo:
   ```bash
   cd "/home/diez/Documentos/Default Project"
   git remote add origin https://github.com/YOURUSER/cosmic-universal-fixes.git
   git branch -M main
   git push -u origin main
   ```

3. Now comment on COSMIC issues (copy-paste English reports):
   - Display: https://github.com/pop-os/cosmic-comp/issues/2336
     Paste content of `.github/BUG_REPORT_DISPLAY_EN.md` + link to your repo `https://github.com/YOURUSER/cosmic-universal-fixes` + patch `0001-*.patch`
   - PiP: https://github.com/pop-os/cosmic-epoch/issues/1526
     Paste `.github/BUG_REPORT_PIP_EN.md`

4. Optional: Open PR in `pop-os/cosmic-comp`:
   - Fork https://github.com/pop-os/cosmic-comp → `YOURUSER/cosmic-comp`
   - In your fork, create branch `fix/hardware-agnostic-flicker-pip-rust` and push patch:
     ```bash
     git clone https://github.com/YOURUSER/cosmic-comp
     cd cosmic-comp
     git checkout -b fix/hardware-agnostic-flicker-pip-rust
     cp -r "/home/diez/Documentos/Default Project/cosmic-display-fix" ./
     cp -r "/home/diez/Documentos/Default Project/cosmic-pip-fix" ./
     git add cosmic-display-fix cosmic-pip-fix
     git commit -m "fix: hardware-agnostic flicker + PiP (Rust, XDG)"
     git push origin fix/hardware-agnostic-flicker-pip-rust
     ```
   - On GitHub, click `Compare & pull request` → body: paste `.github/PULL_REQUEST_TEMPLATE.md`

## If you have `gh` installed and authenticated

```bash
gh repo create cosmic-universal-fixes --public --source="/home/diez/Documentos/Default Project" --remote=origin --push
gh issue comment 2336 --repo pop-os/cosmic-comp --body-file .github/BUG_REPORT_DISPLAY_EN.md
gh issue comment 1526 --repo pop-os/cosmic-epoch --body-file .github/BUG_REPORT_PIP_EN.md
```

