import React from 'react';
import Layout from '@theme/Layout';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';

import HeroBanner from '@site/src/components/HeroBanner';
import FeatureGrid from '@site/src/components/FeatureGrid';
import CodeShowcase from '@site/src/components/CodeShowcase';
import InstallTabs from '@site/src/components/InstallTabs';
import EcosystemGrid from '@site/src/components/EcosystemGrid';

import BoltIcon from '@site/static/img/icons/bolt.svg';
import SparkleIcon from '@site/static/img/icons/sparkle.svg';
import AgentIcon from '@site/static/img/icons/agent.svg';
import DataIcon from '@site/static/img/icons/data.svg';
import BrainIcon from '@site/static/img/icons/brain.svg';
import TestIcon from '@site/static/img/icons/test.svg';
import PlugIcon from '@site/static/img/icons/plug.svg';
import BookIcon from '@site/static/img/icons/book.svg';
import RocketIcon from '@site/static/img/icons/rocket.svg';
import WrenchIcon from '@site/static/img/icons/wrench.svg';

const FEATURES = [
  {
    icon: <SparkleIcon />,
    title: 'Static types, low ceremony',
    description:
      'Type inference covers most annotations. Bindings are immutable by default. Programs run top to bottom with no main function and no class wrapper.',
    bullets: [
      'let / var with full inference',
      'union types for nullable values',
      'expression-oriented syntax',
    ],
  },
  {
    icon: <AgentIcon />,
    title: 'Agents in the language',
    description:
      'agent, stream, and intent are keywords, not a framework. Build event-driven systems without a message bus or actor library.',
    bullets: [
      'agent blocks with on handlers',
      'typed stream events',
      'intent endpoints exposed as MCP tools',
    ],
  },
  {
    icon: <BrainIcon />,
    title: 'AI as a primitive',
    description:
      'Call language models with a generate block. Configure providers with a model declaration. Tool calling and structured output need no SDK.',
    bullets: [
      'generate text and embedding',
      'tool definitions with description fields',
      'structured output via as json',
    ],
  },
  {
    icon: <DataIcon />,
    title: 'Datasets in the language',
    description:
      'Query lists with from / where / select / join. Read and write CSV, JSON, JSONL, and YAML with one keyword.',
    bullets: [
      'from … where … select / join',
      'load and save in 4 formats',
      'composes with map / filter / reduce',
    ],
  },
  {
    icon: <BoltIcon />,
    title: 'Bytecode VM',
    description:
      'Compiles to compact bytecode with constant folding and liveness-based dead-code elimination. One static binary, no runtime dependency.',
    bullets: [
      'compact bytecode',
      'AOT or interpreted',
      'one static binary',
    ],
  },
  {
    icon: <TestIcon />,
    title: 'Tests next to code',
    description:
      'test and expect blocks live alongside the code they cover. No framework, no separate runner. Run mochi test and the tests run.',
    bullets: [
      'test "name" { … } co-located',
      'expect with rich diffs',
      'runs from the same toolchain',
    ],
  },
];

const SAMPLES = [
  {
    label: 'Hello, Mochi',
    hint: 'first program',
    icon: <RocketIcon />,
    description:
      'No main function, no imports, no class wrapper. Write the program and run it.',
    filename: 'hello.mochi',
    code: `let name = "Mochi"
print("Hello, " + name + "!")

// Strings concatenate with + and interpolate via str()
let answer = 42
print("the answer is " + str(answer))`,
    output: `Hello, Mochi!
the answer is 42`,
  },
  {
    label: 'Types and functions',
    hint: 'static safety, low ceremony',
    icon: <BookIcon />,
    description:
      'Define data with type. Write functions with fun. The compiler infers most annotations and rejects type errors at compile time.',
    filename: 'shapes.mochi',
    code: `type Point {
  x: float
  y: float
}

fun distance(a: Point, b: Point): float {
  let dx = a.x - b.x
  let dy = a.y - b.y
  return (dx * dx + dy * dy) ** 0.5
}

let origin = Point { x: 0.0, y: 0.0 }
let target = Point { x: 3.0, y: 4.0 }
print(distance(origin, target))`,
    output: `5`,
  },
  {
    label: 'Agents and streams',
    hint: 'reactive in 12 lines',
    icon: <AgentIcon />,
    description:
      'Agents hold state and react to events. Streams declare event shapes. Emit publishes one event. No message bus, no actor library.',
    filename: 'inbox.mochi',
    code: `stream Message { from: string, body: string }

agent inbox {
  var unread: int = 0

  on Message as m {
    unread = unread + 1
    print("new from " + m.from)
  }

  intent count(): int {
    return unread
  }
}

let box = inbox {}
emit Message { from: "ada", body: "hi" }
emit Message { from: "lin", body: "hey" }
print("unread = " + str(box.count()))`,
    output: `new from ada
new from lin
unread = 2`,
  },
  {
    label: 'Generative AI',
    hint: 'generate blocks in the grammar',
    icon: <BrainIcon />,
    description:
      'Configure a provider once with a model block. Call any model with the same generate syntax. Structured output is one suffix, as json.',
    filename: 'summarize.mochi',
    code: `model fast {
  provider: "openai"
  name: "gpt-4o-mini"
  temperature: 0.3
}

let summary = generate text {
  model: "fast"
  prompt: "Explain bytecode in two sentences."
}
print(summary)

let plan = generate text {
  prompt: "Output a plan with title and steps."
} as json
print(plan["title"])`,
  },
  {
    label: 'Datasets',
    hint: 'queries over plain lists',
    icon: <DataIcon />,
    description:
      'load and save handle CSV, JSON, JSONL, and YAML. from / where / select runs SQL-shaped queries over any list of records.',
    filename: 'top-products.mochi',
    code: `type Product {
  name: string
  price: int
}

let products = load "products.json" as Product

let top = from p in products
          where p.price >= 100
          sort by -p.price
          take 3
          select { name: p.name, price: p.price }

for item in top {
  print(item.name + ", $" + str(item.price))
}

save top to "top.json"`,
    output: `Laptop, $1500
Phone, $900
Tablet, $600`,
  },
  {
    label: 'Tests',
    hint: 'co-located, zero setup',
    icon: <TestIcon />,
    description:
      'Tests live next to the code they cover. mochi test runs every test block in a file or directory tree.',
    filename: 'math.mochi',
    code: `fun add(a: int, b: int): int {
  return a + b
}

fun safe_div(a: int, b: int): int | nil {
  if b == 0 { return nil }
  return a / b
}

test "add is commutative" {
  expect add(2, 3) == add(3, 2)
}

test "safe_div guards zero" {
  expect safe_div(10, 2) == 5
  expect safe_div(10, 0) == nil
}`,
    output: `2 tests passed`,
  },
];

const INSTALL_OPTIONS = [
  {
    label: 'Binary',
    icon: <BoltIcon />,
    recommended: true,
    description:
      'Download a single static binary for your platform. The fastest path from zero to running Mochi code.',
    notes: [
      'Works on macOS (Intel and Apple Silicon) and Linux.',
      'No runtime, no dependencies. Drop it into /usr/local/bin and run.',
    ],
    steps: [
      {
        heading: 'Run the install script',
        language: 'bash',
        code: `curl -fsSL get.mochi-lang.dev | sh`,
      },
      {
        heading: 'Verify the install',
        language: 'bash',
        code: `mochi --version
mochi run -e 'print("ready")'`,
      },
    ],
  },
  {
    label: 'Docker',
    icon: <PlugIcon />,
    description:
      'Run Mochi anywhere Docker runs. Useful for CI pipelines and ephemeral environments.',
    notes: [
      'Image is published at ghcr.io/mochilang/mochi.',
      'Mount the current directory and run any local file.',
    ],
    steps: [
      {
        heading: 'Pull the image',
        language: 'bash',
        code: `docker pull ghcr.io/mochilang/mochi:latest`,
      },
      {
        heading: 'Run a file',
        language: 'bash',
        code: `docker run --rm -i \\
  -v "$PWD:/work" -w /work \\
  ghcr.io/mochilang/mochi run hello.mochi`,
      },
      {
        heading: 'Optional shell alias',
        language: 'bash',
        code: `alias mochi='docker run --rm -i -v "$PWD:/work" -w /work ghcr.io/mochilang/mochi'`,
      },
    ],
  },
  {
    label: 'From source',
    icon: <WrenchIcon />,
    description:
      'Build from source to hack on the language, contribute, or run a feature branch.',
    notes: [
      'Requires Go 1.21 or newer and GNU make.',
      'make install also installs Deno for the TypeScript test corpus.',
    ],
    steps: [
      {
        heading: 'Clone and build',
        language: 'bash',
        code: `git clone https://github.com/mochilang/mochi
cd mochi
make build`,
      },
      {
        heading: 'Put it on your PATH',
        language: 'bash',
        code: `sudo install -m 0755 bin/mochi /usr/local/bin/mochi
mochi --version`,
      },
    ],
  },
];

const ECOSYSTEM = [
  {
    kind: 'Manual',
    title: 'Mochi Manual',
    description:
      'A tour of the language: variables, functions, types, agents, AI generation, and datasets.',
    url: '/docs/manual/',
    linkText: 'Read the manual',
  },
  {
    kind: 'Reference',
    title: 'Language reference',
    description:
      'Concise grammar, operator table, statement reference, and an index of every built-in function.',
    url: '/docs/reference/',
    linkText: 'Open the reference',
  },
  {
    kind: 'Tutorial',
    title: 'Build your first program',
    description:
      'Walk through a complete Mochi program from a blank file: types, functions, tests, and a CLI entry point.',
    url: '/docs/manual/get-started',
    linkText: 'Start the tutorial',
  },
  {
    kind: 'Examples',
    title: 'Examples',
    description:
      'Hundreds of small programs covering agents, datasets, AI, algorithms, and transpilation targets.',
    url: 'https://github.com/mochilang/mochi/tree/main/examples',
    linkText: 'Browse on GitHub',
    external: true,
  },
  {
    kind: 'Roadmap',
    title: 'Roadmap',
    description:
      'Phased plan covering the core language, tooling, transpilation targets, and the path to v1.0.',
    url: '/docs/roadmap',
    linkText: 'See the roadmap',
  },
  {
    kind: 'Community',
    title: 'Discussions and issues',
    description:
      'Ask questions, share what you build, and report bugs in the open with the rest of the community.',
    url: 'https://github.com/mochilang/mochi/discussions',
    linkText: 'Join the discussion',
    external: true,
  },
];

export default function Home() {
  const { siteConfig } = useDocusaurusContext();
  return (
    <Layout
      title={`${siteConfig.title}: a small statically typed language for scripts, agents, and AI tools`}
      description={siteConfig.tagline}
    >
      <HeroBanner
        eyebrow={
          <>
            <span style={{ display: 'inline-block', width: 6, height: 6, borderRadius: 99, background: 'currentColor' }} />
            v0.10 ships agents, streams, and datasets in one binary
          </>
        }
        title={
          <>
            A small language for <span className="accent">scripts, agents, and AI tools.</span>
          </>
        }
        tagline="Mochi is a statically typed, expression-oriented language. It compiles to compact bytecode, ships agents and streams as keywords, and treats AI generation as a first-order construct."
        primaryCta={{ url: '/docs/manual/quickstart', label: 'Get started' }}
        secondaryCta={{ url: '/docs/manual/', label: 'Read the manual' }}
      />

      <FeatureGrid
        title="What Mochi gives you"
        lede="Six properties that the language and toolchain commit to. Every other feature is built on top of these."
        features={FEATURES}
      />

      <CodeShowcase
        title="Six programs end to end"
        lede="Each tab is a complete Mochi file. Copy, paste, and run."
        samples={SAMPLES}
      />

      <InstallTabs
        title="Install"
        lede="Pick the path that fits your environment. Mochi ships as a single binary, in a Docker image, and as source."
        options={INSTALL_OPTIONS}
      />

      <EcosystemGrid
        title="Next steps"
        lede="Mochi is small enough to read in an afternoon. These are the next places to go."
        items={ECOSYSTEM}
      />
    </Layout>
  );
}
