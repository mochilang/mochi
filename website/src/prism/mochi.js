// Prism grammar for Mochi.
// Registers itself on the active Prism instance (relies on the prism-include-
// languages swizzle that mounts Prism on globalThis before requiring this file).

(function (Prism) {
  if (!Prism) return;

  Prism.languages.mochi = {
    'comment': [
      {
        pattern: /\/\*[\s\S]*?\*\//,
        greedy: true,
      },
      {
        pattern: /\/\/.*/,
        greedy: true,
      },
    ],

    'string': {
      pattern: /"(?:\\.|[^"\\\r\n])*"/,
      greedy: true,
      inside: {
        'interpolation': {
          pattern: /\$\{[^}]*\}/,
          inside: {
            'interpolation-punctuation': {
              pattern: /^\$\{|\}$/,
              alias: 'punctuation',
            },
            rest: null, // filled below
          },
        },
        'escape': /\\(?:[\\nrt"0]|x[0-9a-fA-F]{2}|u\{[0-9a-fA-F]+\})/,
      },
    },

    'number': /\b(?:0x[\da-fA-F][\da-fA-F_]*|0b[01][01_]*|\d[\d_]*(?:\.\d[\d_]*)?(?:[eE][+-]?\d+)?)\b/,

    'keyword':
      /\b(?:let|var|fun|return|if|else|then|match|for|in|while|break|continue|type|is|as|import|export|package|test|expect|from|where|select|sort|by|group|having|union|intersect|except|all|load|save|to|agent|on|emit|event|stream|generate|text|embedding|model|with|method|self|panic|defer|require|when|enum)\b/,

    'boolean': /\b(?:true|false|nil)\b/,

    'builtin':
      /\b(?:int|float|bool|string|void|any|list|map|set)\b/,

    'function-call': {
      pattern: /\b[a-zA-Z_][\w]*(?=\s*\()/,
      alias: 'function',
    },

    'class-name': {
      pattern: /\b[A-Z][\w]*\b/,
    },

    'operator': /\?\?|\?\.|\.\.=|\.\.|=>|->|==|!=|<=|>=|&&|\|\||\*\*=?|\+=|-=|\*=|\/=|%=|=|\+|-|\*|\/|%|<|>|!|\||&/,

    'punctuation': /[{}[\]();,.:]/,
  };

  // Wire the string interpolation rest to the full mochi grammar (recursive).
  Prism.languages.mochi['string'].inside['interpolation'].inside.rest =
    Prism.languages.mochi;

  // Common alias.
  Prism.languages.mc = Prism.languages.mochi;
})(typeof globalThis !== 'undefined' ? globalThis.Prism : undefined);
