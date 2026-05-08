import React from 'react';
import Link from '@docusaurus/Link';
import styles from './styles.module.css';

const HERO_CODE_LINES = [
  { line: '// no main() needed — programs run top to bottom', cls: 'comment' },
  { line: 'let greeting = "Hello, " + name + "!"', cls: '' },
  { line: 'print(greeting)', cls: '' },
  { line: '', cls: '' },
  { line: '// strongly typed — but inference does the work', cls: 'comment' },
  { line: 'fun greet(user: string): string {', cls: '' },
  { line: '  return "welcome, " + user', cls: '' },
  { line: '}', cls: '' },
  { line: '', cls: '' },
  { line: '// agents react to events', cls: 'comment' },
  { line: 'agent inbox {', cls: '' },
  { line: '  on message(text: string) {', cls: '' },
  { line: '    emit reply(greet(text))', cls: '' },
  { line: '  }', cls: '' },
  { line: '}', cls: '' },
];

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
          <div className={styles.heroCode} aria-hidden="true">
            <div className={styles.codeWindow}>
              <div className={styles.codeChrome}>
                <span className={styles.codeDot} style={{ background: '#ff5f57' }} />
                <span className={styles.codeDot} style={{ background: '#febc2e' }} />
                <span className={styles.codeDot} style={{ background: '#28c840' }} />
                <span className={styles.codeFile}>hello.mochi</span>
              </div>
              <pre className={styles.codeBody}>
                {HERO_CODE_LINES.map((row, i) => (
                  <div key={i} className={styles.codeLine}>
                    <span className={styles.codeLineNum}>{i + 1}</span>
                    <span className={row.cls === 'comment' ? styles.codeComment : ''}>
                      {row.line || ' '}
                    </span>
                  </div>
                ))}
              </pre>
            </div>
          </div>
        </div>
      </div>
    </header>
  );
}
