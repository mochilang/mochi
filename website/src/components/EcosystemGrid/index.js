import React from 'react';
import Link from '@docusaurus/Link';
import styles from './styles.module.css';

export default function EcosystemGrid({ title, lede, items }) {
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
          {items.map((item) => {
            const Component = item.external ? 'a' : Link;
            const props = item.external
              ? { href: item.url, target: '_blank', rel: 'noopener noreferrer' }
              : { to: item.url };
            return (
              <Component key={item.title} className={styles.card} {...props}>
                <span className={styles.cardKind}>{item.kind}</span>
                <h3 className={styles.cardTitle}>{item.title}</h3>
                <p className={styles.cardDesc}>{item.description}</p>
                <span className={styles.cardLink}>
                  {item.linkText || 'Learn more'}
                  <svg
                    width="12"
                    height="12"
                    viewBox="0 0 12 12"
                    fill="none"
                    stroke="currentColor"
                    strokeWidth="1.6"
                    strokeLinecap="round"
                    strokeLinejoin="round"
                  >
                    <path d="M3 6h6M6 3l3 3-3 3" />
                  </svg>
                </span>
              </Component>
            );
          })}
        </div>
      </div>
    </section>
  );
}
