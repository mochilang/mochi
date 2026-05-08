import React from 'react';
import clsx from 'clsx';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import CodeBlock from '@theme/CodeBlock';
import styles from './index.module.css';

const HERO_CODE = `\
// No main() needed — programs run top to bottom
let name = "world"
print("Hello, " + name + "!")

// Functions with type annotations
fun greet(user: string): string {
  return "Welcome to Mochi, " + user + "!"
}

// Agents handle events reactively
agent greeter {
  on message(text: string) {
    emit reply(greet(text))
  }
}`;

const FEATURES = [
  {
    icon: '🧹',
    title: 'Clean, expressive syntax',
    description:
      'Statically typed with Python-like clarity. No boilerplate, no ceremony. Programs run top to bottom — just write code.',
  },
  {
    icon: '🤖',
    title: 'Agent-native by design',
    description:
      'First-class agent and stream blocks let you build reactive, event-driven systems without external frameworks.',
  },
  {
    icon: '⚡',
    title: 'Fast bytecode VM',
    description:
      'Optimized bytecode with constant folding and liveness-based dead code elimination. Single binary, zero dependencies.',
  },
  {
    icon: '🧠',
    title: 'Built-in AI generation',
    description:
      'Native generate blocks connect to AI models. Produce text, JSON, or structured data without external SDKs.',
  },
  {
    icon: '🧪',
    title: 'Testable by design',
    description:
      'Built-in test and expect blocks make it easy to test your code as you write it — no testing framework needed.',
  },
  {
    icon: '🔌',
    title: 'Interoperable',
    description:
      'Transpiles to Go, Python, and TypeScript. FFI support lets you call native libraries. Use Mochi alongside any stack.',
  },
];

const CODE_TABS = [
  {
    label: 'Hello World',
    code: `let name = "world"
print("Hello, " + name + "!")`,
    lang: 'mochi',
  },
  {
    label: 'Functions & Types',
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
let p = Point { x: 3.0, y: 4.0 }
print(distance(origin, p))  // 5.0`,
    lang: 'mochi',
  },
  {
    label: 'Agents & Streams',
    code: `agent counter {
  var count = 0

  on increment() {
    count = count + 1
    emit updated(count)
  }

  on reset() {
    count = 0
    emit updated(count)
  }
}

stream numbers {
  for i in 1..10 {
    emit value(i)
  }
}`,
    lang: 'mochi',
  },
  {
    label: 'Generative AI',
    code: `model gpt {
  provider: "openai"
  name: "gpt-4o-mini"
  temperature: 0.7
}

let summary = generate text {
  model: "gpt"
  prompt: "Summarize the history of computing in 3 sentences."
}

print(summary)

// Structured output
let tags = generate text {
  prompt: "List 5 programming language keywords as JSON array"
} as json`,
    lang: 'mochi',
  },
  {
    label: 'Tests',
    code: `fun add(a: int, b: int): int {
  return a + b
}

test "addition" {
  expect add(1, 2) == 3
  expect add(-1, 1) == 0
  expect add(0, 0) == 0
}

test "string concat" {
  let s = "Hello" + ", " + "Mochi!"
  expect s == "Hello, Mochi!"
}`,
    lang: 'mochi',
  },
];

function FeatureCard({ icon, title, description }) {
  return (
    <div className={clsx('col col--4', styles.featureCol)}>
      <div className="feature-card">
        <div className={styles.featureIcon}>{icon}</div>
        <h3 className={styles.featureTitle}>{title}</h3>
        <p className={styles.featureDesc}>{description}</p>
      </div>
    </div>
  );
}

function CodeShowcase() {
  const [active, setActive] = React.useState(0);
  return (
    <div className={styles.codeShowcase}>
      <div className={styles.codeTabBar}>
        {CODE_TABS.map((t, i) => (
          <button
            key={t.label}
            className={clsx(styles.codeTab, i === active && styles.codeTabActive)}
            onClick={() => setActive(i)}
          >
            {t.label}
          </button>
        ))}
      </div>
      <CodeBlock language={CODE_TABS[active].lang} className={styles.codeBlock}>
        {CODE_TABS[active].code}
      </CodeBlock>
    </div>
  );
}

export default function Home() {
  const { siteConfig } = useDocusaurusContext();
  return (
    <Layout title="Mochi Programming Language" description={siteConfig.tagline}>
      {/* Hero */}
      <header className={clsx('hero hero--primary', styles.heroBanner)}>
        <div className="container">
          <div className={styles.heroInner}>
            <div className={styles.heroText}>
              <div className={styles.heroBadge}>🍡 Mochi Language</div>
              <h1 className={styles.heroTitle}>
                Simple. Safe.<br />Agent-native.
              </h1>
              <p className={styles.heroSubtitle}>{siteConfig.tagline}</p>
              <div className={styles.heroCtas}>
                <Link className="button button--primary button--lg" to="/docs/manual/quickstart">
                  Get started →
                </Link>
                <Link className="button button--secondary button--lg" to="/docs/manual/">
                  Read the manual
                </Link>
              </div>
              <div className={styles.heroInstall}>
                <code>curl -fsSL https://mochilang.github.io/mochi/install.sh | sh</code>
              </div>
            </div>
            <div className={styles.heroCode}>
              <pre className="homepage-hero-code">{HERO_CODE}</pre>
            </div>
          </div>
        </div>
      </header>

      <main>
        {/* Features */}
        <section className={clsx('homepage-section', styles.featuresSection)}>
          <div className="container">
            <h2 className={styles.sectionTitle}>Why Mochi?</h2>
            <div className="row">
              {FEATURES.map((f) => (
                <FeatureCard key={f.title} {...f} />
              ))}
            </div>
          </div>
        </section>

        {/* Code showcase */}
        <section className={clsx('homepage-section homepage-section--alt', styles.showcaseSection)}>
          <div className="container">
            <h2 className={styles.sectionTitle}>See it in action</h2>
            <p className={styles.sectionSubtitle}>
              Mochi is simple enough to explore in minutes, powerful enough to build real systems.
            </p>
            <CodeShowcase />
          </div>
        </section>

        {/* Install */}
        <section className={clsx('homepage-section', styles.installSection)}>
          <div className="container">
            <h2 className={styles.sectionTitle}>Install Mochi</h2>
            <p className={styles.sectionSubtitle}>Three ways to get started.</p>
            <div className="row">
              <div className="col col--4">
                <div className="feature-card">
                  <h3>Binary</h3>
                  <p>Download the prebuilt binary for your platform — no setup required.</p>
                  <CodeBlock language="bash">
                    {`curl -fsSL https://mochilang.github.io/mochi/install.sh | sh\nmochi run examples/hello.mochi`}
                  </CodeBlock>
                </div>
              </div>
              <div className="col col--4">
                <div className="feature-card">
                  <h3>Docker</h3>
                  <p>Run Mochi anywhere Docker is available.</p>
                  <CodeBlock language="bash">
                    {`docker run -i --rm ghcr.io/mochilang/mochi run examples/hello.mochi`}
                  </CodeBlock>
                </div>
              </div>
              <div className="col col--4">
                <div className="feature-card">
                  <h3>From source</h3>
                  <p>Build and hack on the language itself.</p>
                  <CodeBlock language="bash">
                    {`git clone https://github.com/mochilang/mochi\ncd mochi && make build\nmochi run examples/hello.mochi`}
                  </CodeBlock>
                </div>
              </div>
            </div>
            <div className={styles.installCta}>
              <Link className="button button--primary button--lg" to="/docs/manual/quickstart">
                Full installation guide
              </Link>
            </div>
          </div>
        </section>

        {/* Community */}
        <section className={clsx('homepage-section homepage-section--alt', styles.communitySection)}>
          <div className="container" style={{ textAlign: 'center' }}>
            <h2 className={styles.sectionTitle}>Join the community</h2>
            <p className={styles.sectionSubtitle}>
              Mochi is open source and built in the open. Contributions are welcome.
            </p>
            <div className={styles.communityLinks}>
              <Link className="button button--secondary button--lg" href="https://github.com/mochilang/mochi">
                GitHub
              </Link>
              <Link className="button button--secondary button--lg" href="https://github.com/mochilang/mochi/issues">
                Report an issue
              </Link>
              <Link className="button button--secondary button--lg" href="https://github.com/mochilang/mochi/discussions">
                Discussions
              </Link>
            </div>
          </div>
        </section>
      </main>
    </Layout>
  );
}
