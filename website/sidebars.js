/** @type {import('@docusaurus/plugin-content-docs').SidebarsConfig} */
const sidebars = {
  manualSidebar: [
    'manual/index',
    {
      label: 'Get started',
      type: 'category',
      className: 'sidebar-heading',
      collapsed: false,
      items: [
        'manual/quickstart',
        'manual/get-started',
        'manual/requirements',
      ],
    },
    {
      label: 'Language basics',
      type: 'category',
      className: 'sidebar-heading',
      collapsed: false,
      items: [
        'manual/basics',
        'manual/variables',
        'manual/functions',
        'manual/types',
        'manual/operators',
        'manual/control-flow',
        'manual/errors',
        'manual/packages',
      ],
    },
    {
      label: 'Agents & Streams',
      type: 'category',
      className: 'sidebar-heading',
      collapsed: false,
      items: [
        'manual/agents',
        'manual/streams',
      ],
    },
    {
      label: 'Generative AI',
      type: 'category',
      className: 'sidebar-heading',
      collapsed: false,
      items: [
        'manual/generative-ai',
        'manual/datasets',
      ],
    },
  ],

  referenceSidebar: [
    'reference/index',
    'reference/keywords',
    'reference/operators',
    'reference/builtins',
  ],
};

module.exports = sidebars;
