# Git Workflow for ConsensusConnectR

## Repository Information

- **Remote URL:** https://github.com/AJMcGovernLab/ConsensusConnectR.git
- **Default Branch:** main
- **Local Path:** `/home/ajukearth/ConsensusConnectR/Persistence6`

## Common Git Commands

### Check Status

Before making any changes, check the current state:

```bash
git status
```

### Pull Latest Changes

Always pull before starting work to avoid conflicts:

```bash
git pull origin main
```

### Stage Changes

Stage specific files:

```bash
git add <filename>
```

Stage all changes:

```bash
git add .
```

### Commit Changes

Create a commit with a descriptive message:

```bash
git commit -m "Brief description of changes"
```

### Push Changes

Push commits to the remote repository:

```bash
git push origin main
```

## Typical Workflow

1. **Pull latest changes:**
   ```bash
   git pull origin main
   ```

2. **Make your edits** to the code

3. **Check what changed:**
   ```bash
   git status
   git diff
   ```

4. **Stage and commit:**
   ```bash
   git add .
   git commit -m "Describe what you changed"
   ```

5. **Push to GitHub:**
   ```bash
   git push origin main
   ```

## Important Notes

- Always run git commands from the `/home/ajukearth/ConsensusConnectR/Persistence6` directory
- Write clear, concise commit messages that describe the change
- Pull before pushing to avoid merge conflicts
- Do not commit sensitive data (API keys, passwords, etc.)

## Viewing History

See recent commits:

```bash
git log --oneline -10
```

See changes in a specific commit:

```bash
git show <commit-hash>
```

## Undoing Changes

Discard unstaged changes to a file:

```bash
git checkout -- <filename>
```

Unstage a file (keep changes):

```bash
git reset HEAD <filename>
```
