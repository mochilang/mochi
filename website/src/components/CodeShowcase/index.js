import React, { useState } from 'react';
import CodeBlock from '@theme/CodeBlock';
import styles from './styles.module.css';

export default function CodeShowcase({ title, lede, samples }) {
  const [active, setActive] = useState(0);
  const sample = samples[active];

  return (
    <section className={styles.section}>
      <div className="container">
        {title ? (
          <header className={styles.header}>
            <h2 className={styles.title}>{title}</h2>
            {lede ? <p className={styles.lede}>{lede}</p> : null}
          </header>
        ) : null}

        <div className={styles.shell}>
          <aside className={styles.sidebar}>
            {samples.map((s, i) => (
              <button
                key={s.label}
                type="button"
                className={
                  i === active ? `${styles.tab} ${styles.tabActive}` : styles.tab
                }
                onClick={() => setActive(i)}
              >
                <span className={styles.tabIcon}>{s.icon}</span>
                <span className={styles.tabBody}>
                  <span className={styles.tabLabel}>{s.label}</span>
                  <span className={styles.tabHint}>{s.hint}</span>
                </span>
              </button>
            ))}
          </aside>

          <div className={styles.viewport}>
            <div className={styles.viewportHeader}>
              <h3 className={styles.viewportTitle}>{sample.label}</h3>
              <p className={styles.viewportHint}>{sample.description}</p>
            </div>
            <div className={styles.codeWrap}>
              <CodeBlock
                language={sample.language || 'mochi'}
                title={sample.filename}
                showLineNumbers
              >
                {sample.code}
              </CodeBlock>
            </div>
            {sample.input ? (
              <div className={styles.inputWrap}>
                <div className={styles.outputLabel}>Input</div>
                <CodeBlock
                  language={sample.input.language || 'json'}
                  title={sample.input.filename}
                >
                  {sample.input.code}
                </CodeBlock>
              </div>
            ) : null}
            {sample.output ? (
              <div className={styles.outputWrap}>
                <div className={styles.outputLabel}>Output</div>
                <CodeBlock language="text">{sample.output}</CodeBlock>
              </div>
            ) : null}
            {sample.alt ? (
              <div className={styles.altWrap}>
                <div className={styles.outputLabel}>
                  {sample.alt.heading || 'Or as one expression'}
                </div>
                <CodeBlock language={sample.alt.language || 'mochi'}>
                  {sample.alt.code}
                </CodeBlock>
                {sample.alt.output ? (
                  <CodeBlock language="text">{sample.alt.output}</CodeBlock>
                ) : null}
              </div>
            ) : null}
          </div>
        </div>
      </div>
    </section>
  );
}
