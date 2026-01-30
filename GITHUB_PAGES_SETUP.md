# GitHub Pages Setup Guide

This guide will help you deploy the Secret Orb website to GitHub Pages.

## Quick Setup

### 1. Update Configuration

Edit `_config.yml` and update these fields:

```yaml
author: Your Name
url: https://yourusername.github.io
baseurl: /secret-orb
```

Replace `yourusername` with your GitHub username and `/secret-orb` with your repository name.

### 2. Enable GitHub Pages

1. Go to your repository on GitHub
2. Click **Settings** → **Pages** (in left sidebar)
3. Under "Source", select:
   - **Branch:** `main` (or your default branch)
   - **Folder:** `/ (root)`
4. Click **Save**

### 3. Wait for Deployment

GitHub will automatically build and deploy your site. This takes 1-3 minutes.

You can check the deployment status under the **Actions** tab.

### 4. View Your Site

Once deployed, your site will be available at:
```
https://yourusername.github.io/secret-orb/
```

## File Structure

```
├── _config.yml              # Jekyll configuration
├── _layouts/
│   └── default.html         # Custom retro terminal layout
├── assets/
│   └── css/
│       └── style.css        # CRT monitor styling
├── index.md                 # Homepage (includes README.md)
└── README.md                # Main content
```

## Customization

### Change Color Scheme

Edit `assets/css/style.css` and modify the `:root` variables:

**Green Phosphor (default):**
```css
--text-primary: #33ff33;
--text-secondary: #00cc00;
--accent: #00ff00;
```

**Amber Monitor:**
```css
--text-primary: #ffb000;
--text-secondary: #ff8800;
--accent: #ffcc00;
```

**White/Gray:**
```css
--text-primary: #e0e0e0;
--text-secondary: #a0a0a0;
--accent: #ffffff;
```

### Disable CRT Effects

To disable scanlines and flicker effects, comment out or remove these sections in `style.css`:
- `.crt-overlay`
- `.scanlines`

### Add Your Logo

Add an image to the repository and include it in `README.md`:
```markdown
![Secret Orb Logo](assets/images/logo.png)
```

## Testing Locally

To test your site locally before deploying:

### Install Jekyll

```bash
# Install Ruby and Bundler first, then:
gem install jekyll bundler
```

### Create Gemfile

Create a file named `Gemfile` in the root:

```ruby
source 'https://rubygems.org'
gem 'github-pages', group: :jekyll_plugins
```

### Run Local Server

```bash
bundle install
bundle exec jekyll serve
```

Visit `http://localhost:4000` in your browser.

## Troubleshooting

### Site Not Loading

1. Check that GitHub Pages is enabled in Settings → Pages
2. Verify the branch and folder are correct
3. Check the Actions tab for build errors

### CSS Not Loading

1. Verify `baseurl` in `_config.yml` matches your repository name
2. Check that file paths use `{{ '/assets/css/style.css' | relative_url }}`

### 404 Error

1. Make sure `index.md` exists in the root
2. Verify the repository is public (or you have GitHub Pro for private repos)

### Build Failures

Check the Actions tab for detailed error messages. Common issues:
- Invalid YAML in `_config.yml`
- Missing required files
- Liquid template syntax errors

## Custom Domain (Optional)

To use a custom domain like `secretorb.com`:

1. Buy domain from a registrar
2. Add a `CNAME` file to repository root with your domain:
   ```
   secretorb.com
   ```
3. Configure DNS records at your registrar:
   - Add A records pointing to GitHub's IPs
   - Or add CNAME record pointing to `yourusername.github.io`
4. In GitHub Settings → Pages, enter your custom domain
5. Enable "Enforce HTTPS"

See [GitHub's custom domain guide](https://docs.github.com/en/pages/configuring-a-custom-domain-for-your-github-pages-site) for details.

## Need Help?

- [GitHub Pages Documentation](https://docs.github.com/en/pages)
- [Jekyll Documentation](https://jekyllrb.com/docs/)
- [Create an issue](https://github.com/yourusername/secret-orb/issues)
