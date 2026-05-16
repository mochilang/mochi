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

  mepSidebar: [
    'mep/index',
    'mep/mep-0000',
    {
      label: 'Foundations',
      type: 'category',
      className: 'sidebar-heading',
      collapsed: false,
      items: [
        'mep/mep-0001',
        'mep/mep-0002',
        'mep/mep-0003',
        'mep/mep-0004',
        'mep/mep-0005',
        'mep/mep-0006',
      ],
    },
    {
      label: 'Soundness',
      type: 'category',
      className: 'sidebar-heading',
      collapsed: false,
      items: [
        'mep/mep-0007',
        'mep/mep-0008',
        'mep/mep-0009',
        'mep/mep-0010',
      ],
    },
    {
      label: 'Standards Track',
      type: 'category',
      className: 'sidebar-heading',
      collapsed: false,
      items: [
        'mep/mep-0011',
        'mep/mep-0012',
        'mep/mep-0013',
        'mep/mep-0014',
        'mep/mep-0015',
        'mep/mep-0016',
      ],
    },
    {
      label: 'VM & Runtime',
      type: 'category',
      className: 'sidebar-heading',
      collapsed: false,
      items: [
        'mep/mep-0017',
        'mep/mep-0018',
        'mep/mep-0019',
        'mep/mep-0020',
        'mep/mep-0021',
        'mep/mep-0022',
        'mep/mep-0023',
        'mep/mep-0024',
        'mep/mep-0025',
        'mep/mep-0026',
        'mep/mep-0027',
        'mep/mep-0028',
      ],
    },
  ],
};

module.exports = sidebars;
