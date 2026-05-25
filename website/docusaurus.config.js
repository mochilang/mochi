// @ts-check
const { themes } = require('prism-react-renderer');

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'Mochi',
  tagline:
    'A small, statically typed language for scripting, agents, and AI-augmented tools.',
  favicon: 'img/favicon.svg',

  url: 'https://mochi-lang.dev',
  baseUrl: '/',

  organizationName: 'mochilang',
  projectName: 'mochi',
  trailingSlash: false,

  onBrokenLinks: 'warn',
  onBrokenMarkdownLinks: 'warn',

  // Treat .md files as CommonMark and .mdx files as MDX. This keeps
  // MEP prose (lots of `list<T>` / `map<K,V>` and other ASCII art)
  // from tripping the MDX-JSX strict parser, while index.mdx-style
  // files keep their JSX powers.
  markdown: {
    format: 'detect',
  },

  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  stylesheets: [
    {
      href: 'https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&family=Roboto+Mono:wght@400;500;600&display=swap',
      type: 'text/css',
    },
  ],

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          sidebarPath: require.resolve('./sidebars.js'),
          editUrl: 'https://github.com/mochilang/mochi/tree/main/website/',
          breadcrumbs: true,
          showLastUpdateTime: false,
          remarkPlugins: [require('./scripts/remark-mochi-links')],
        },
        blog: false,
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
        sitemap: {
          changefreq: 'weekly',
          priority: 0.7,
        },
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      image: 'img/og-card.png',
      colorMode: {
        defaultMode: 'dark',
        disableSwitch: false,
        respectPrefersColorScheme: true,
      },
      docs: {
        sidebar: {
          hideable: true,
          autoCollapseCategories: false,
        },
      },
      navbar: {
        title: 'Mochi',
        logo: {
          alt: 'Mochi logo',
          src: 'img/logo.svg',
          srcDark: 'img/logo.svg',
          width: 26,
          height: 26,
        },
        hideOnScroll: false,
        items: [
          {
            type: 'docSidebar',
            sidebarId: 'manualSidebar',
            position: 'left',
            label: 'Manual',
          },
          {
            type: 'docSidebar',
            sidebarId: 'referenceSidebar',
            position: 'left',
            label: 'Reference',
          },
          {
            type: 'docSidebar',
            sidebarId: 'mepSidebar',
            position: 'left',
            label: 'MEPs',
          },
          {
            type: 'docSidebar',
            sidebarId: 'researchSidebar',
            position: 'left',
            label: 'Research',
          },
          {
            type: 'docSidebar',
            sidebarId: 'implementationSidebar',
            position: 'left',
            label: 'Implementation',
          },
          {
            to: '/docs/roadmap',
            label: 'Roadmap',
            position: 'left',
          },
          {
            type: 'docSidebar',
            sidebarId: 'changelogSidebar',
            label: 'Changelog',
            position: 'left',
          },
          {
            to: '/docs/faq',
            label: 'FAQ',
            position: 'left',
          },
          {
            href: 'https://github.com/mochilang/mochi',
            label: 'GitHub',
            position: 'right',
          },
        ],
      },
      footer: {
        style: 'light',
        logo: {
          alt: 'Mochi logo',
          src: 'img/logo.svg',
          width: 28,
          height: 28,
        },
        links: [
          {
            title: 'Learn',
            items: [
              { label: 'Quickstart', to: '/docs/manual/quickstart' },
              { label: 'Tutorial', to: '/docs/manual/get-started' },
              { label: 'Manual', to: '/docs/manual/' },
              { label: 'Reference', to: '/docs/reference/' },
            ],
          },
          {
            title: 'Topics',
            items: [
              { label: 'Agents & Streams', to: '/docs/manual/agents' },
              { label: 'Generative AI', to: '/docs/manual/generative-ai' },
              { label: 'Datasets', to: '/docs/manual/datasets' },
              { label: 'Packages', to: '/docs/manual/packages' },
            ],
          },
          {
            title: 'Project',
            items: [
              { label: 'Vision', to: '/docs/vision' },
              { label: 'Roadmap', to: '/docs/roadmap' },
              { label: 'Changelog', to: '/docs/changelog' },
              { label: 'FAQ', to: '/docs/faq' },
            ],
          },
          {
            title: 'Community',
            items: [
              { label: 'GitHub', href: 'https://github.com/mochilang/mochi' },
              {
                label: 'Issues',
                href: 'https://github.com/mochilang/mochi/issues',
              },
              {
                label: 'Discussions',
                href: 'https://github.com/mochilang/mochi/discussions',
              },
              {
                label: 'Releases',
                href: 'https://github.com/mochilang/mochi/releases',
              },
            ],
          },
        ],
        copyright: `© ${new Date().getFullYear()} Mochi Language. Built with Docusaurus.`,
      },
      prism: {
        theme: themes.oneLight,
        darkTheme: themes.oneDark,
        additionalLanguages: [
          'bash',
          'docker',
          'json',
          'yaml',
          'python',
          'go',
          'rust',
          'typescript',
        ],
        magicComments: [
          {
            className: 'theme-code-block-highlighted-line',
            line: 'highlight-next-line',
            block: { start: 'highlight-start', end: 'highlight-end' },
          },
        ],
      },
      algolia: undefined,
      announcementBar: {
        id: 'announcement-v010',
        content:
          'Mochi v0.10 ships agents, streams, datasets, and AI generation in one binary. <a href="/docs/changelog">See what is new →</a>',
        backgroundColor: '#e6f0e1',
        textColor: '#1a1d12',
        isCloseable: true,
      },
    }),
};

module.exports = config;
