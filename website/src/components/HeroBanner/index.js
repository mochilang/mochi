import React from 'react';
import Link from '@docusaurus/Link';
import CodeBlock from '@theme/CodeBlock';
import styles from './styles.module.css';

const HERO_CODE = `// no main needed, programs run top to bottom
let greeting = "Hello, " + name + "!"
print(greeting)

// strongly typed, inference does the work
fun greet(user: string): string {
  return "welcome, " + user
}

// agents react to events
agent inbox {
  on message(text: string) {
    emit reply(greet(text))
  }
}`;

export default function HeroBanner({
  eyebrow,
  title,
  tagline,
  primaryCta,
  secondaryCta,
}) {
  return (
    <header className={styles.hero}>
      <div className={styles.heroBg} aria-hidden="true">
        <div className={styles.heroBgGradient} />
        <div className={styles.heroBgGrid} />
        <div className={styles.heroBgGlow} />
      </div>
      <div className="container">
        <div className={styles.heroInner}>
          <div className={styles.heroText}>
            {eyebrow ? <div className={styles.eyebrow}>{eyebrow}</div> : null}
            <h1 className={styles.title}>{title}</h1>
            <p className={styles.tagline}>{tagline}</p>
            <div className={styles.ctas}>
              {primaryCta ? (
                <Link
                  className="button button--primary button--lg"
                  to={primaryCta.url}
                >
                  {primaryCta.label}
                  <svg
                    aria-hidden="true"
                    width="14"
                    height="14"
                    viewBox="0 0 14 14"
                    style={{ marginLeft: 6, verticalAlign: -1 }}
                  >
                    <path
                      d="M3 7h8M7 3l4 4-4 4"
                      stroke="currentColor"
                      strokeWidth="1.8"
                      fill="none"
                      strokeLinecap="round"
                      strokeLinejoin="round"
                    />
                  </svg>
                </Link>
              ) : null}
              {secondaryCta ? (
                <Link
                  className="button button--secondary button--lg"
                  to={secondaryCta.url}
                >
                  {secondaryCta.label}
                </Link>
              ) : null}
            </div>
            <div className={styles.installLine}>
              <span className={styles.installPrompt}>$</span>
              <code>curl -fsSL get.mochi-lang.dev | sh</code>
              <button
                type="button"
                className={styles.copyBtn}
                onClick={() => {
                  navigator.clipboard?.writeText('curl -fsSL get.mochi-lang.dev | sh');
                }}
                aria-label="Copy install command"
              >
                <svg width="14" height="14" viewBox="0 0 14 14" fill="none" stroke="currentColor" strokeWidth="1.5">
                  <rect x="3" y="3" width="8" height="8" rx="1.5" />
                  <path d="M5 3V2a1 1 0 0 1 1-1h6a1 1 0 0 1 1 1v6a1 1 0 0 1-1 1h-1" />
                </svg>
              </button>
            </div>
          </div>
          <div className={styles.heroCode}>
            <CodeBlock language="mochi" title="hello.mochi" showLineNumbers>
              {HERO_CODE}
            </CodeBlock>
          </div>
        </div>
      </div>
    </header>
  );
}
