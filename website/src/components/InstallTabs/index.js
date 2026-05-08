import React, { useState } from 'react';
import CodeBlock from '@theme/CodeBlock';
import styles from './styles.module.css';

export default function InstallTabs({ title, lede, options }) {
  const [active, setActive] = useState(0);

  return (
    <section className={styles.section}>
      <div className="container">
        {title ? (
          <header className={styles.header}>
            <h2 className={styles.title}>{title}</h2>
            {lede ? <p className={styles.lede}>{lede}</p> : null}
          </header>
        ) : null}

        <div className={styles.tabBar}>
          {options.map((opt, i) => (
            <button
              key={opt.label}
              type="button"
              className={
                i === active ? `${styles.tab} ${styles.tabActive}` : styles.tab
              }
              onClick={() => setActive(i)}
            >
              {opt.icon ? <span className={styles.tabIcon}>{opt.icon}</span> : null}
              <span>{opt.label}</span>
              {opt.recommended ? (
                <span className={styles.recommendedBadge}>recommended</span>
              ) : null}
            </button>
          ))}
        </div>

        <div className={styles.panel}>
          <div className={styles.panelText}>
            <h3 className={styles.panelTitle}>{options[active].label}</h3>
            <p className={styles.panelDesc}>{options[active].description}</p>
            {options[active].notes ? (
              <ul className={styles.panelNotes}>
                {options[active].notes.map((n) => (
                  <li key={n}>{n}</li>
                ))}
              </ul>
            ) : null}
          </div>
          <div className={styles.panelCode}>
            {options[active].steps.map((step, i) => (
              <div key={i} className={styles.step}>
                {step.heading ? (
                  <div className={styles.stepHeading}>
                    <span className={styles.stepNum}>{i + 1}</span>
                    {step.heading}
                  </div>
                ) : null}
                <CodeBlock language={step.language || 'bash'}>
                  {step.code}
                </CodeBlock>
              </div>
            ))}
          </div>
        </div>
      </div>
    </section>
  );
}
