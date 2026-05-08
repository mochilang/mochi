// @ts-check
const { themes } = require('prism-react-renderer');

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'Mochi',
  tagline: 'A small, statically typed language built for clarity, safety, and agent-oriented programming.',
  favicon: 'img/favicon.ico',

  url: 'https://mochilang.github.io',
  baseUrl: '/mochi/',

  organizationName: 'mochilang',
  projectName: 'mochi',
  trailingSlash: false,

  onBrokenLinks: 'warn',
  onBrokenMarkdownLinks: 'warn',

  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          sidebarPath: require.resolve('./sidebars.js'),
          editUrl: 'https://github.com/mochilang/mochi/tree/main/website/',
        },
        blog: false,
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      image: 'img/mochi-social-card.png',
      colorMode: {
        defaultMode: 'dark',
        disableSwitch: false,
        respectPrefersColorScheme: true,
      },
      navbar: {
        title: 'Mochi',
        logo: {
          alt: 'Mochi Logo',
          src: 'img/logo.svg',
          srcDark: 'img/logo.svg',
        },
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
            to: '/docs/roadmap',
            label: 'Roadmap',
            position: 'left',
          },
          {
            to: '/docs/changelog',
            label: 'Changelog',
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
        style: 'dark',
        links: [
          {
            title: 'Docs',
            items: [
              { label: 'Quickstart', to: '/docs/manual/quickstart' },
              { label: 'Manual', to: '/docs/manual/' },
              { label: 'Reference', to: '/docs/reference/' },
            ],
          },
          {
            title: 'Community',
            items: [
              { label: 'GitHub Issues', href: 'https://github.com/mochilang/mochi/issues' },
              { label: 'GitHub Discussions', href: 'https://github.com/mochilang/mochi/discussions' },
            ],
          },
          {
            title: 'More',
            items: [
              { label: 'Changelog', to: '/docs/changelog' },
              { label: 'Roadmap', to: '/docs/roadmap' },
              { label: 'Vision', to: '/docs/vision' },
              { label: 'FAQ', to: '/docs/faq' },
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} Mochi Language. Built with Docusaurus.`,
      },
      prism: {
        theme: themes.vsLight,
        darkTheme: themes.vsDark,
        additionalLanguages: ['bash', 'docker', 'json', 'yaml', 'python', 'go'],
      },
      algolia: undefined,
    }),
};

module.exports = config;
