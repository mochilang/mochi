import React from 'react';
import styles from './styles.module.css';

export default function FeatureGrid({ title, lede, features }) {
  return (
    <section className={styles.section}>
      <div className="container">
        {title ? (
          <header className={styles.header}>
            <h2 className={styles.title}>{title}</h2>
            {lede ? <p className={styles.lede}>{lede}</p> : null}
          </header>
        ) : null}
        <div className={styles.grid}>
          {features.map((f) => (
            <div key={f.title} className={styles.card}>
              <div className={styles.cardIcon}>{f.icon}</div>
              <h3 className={styles.cardTitle}>{f.title}</h3>
              <p className={styles.cardDesc}>{f.description}</p>
              {f.bullets ? (
                <ul className={styles.cardBullets}>
                  {f.bullets.map((b) => (
                    <li key={b}>{b}</li>
                  ))}
                </ul>
              ) : null}
            </div>
          ))}
        </div>
      </div>
    </section>
  );
}
