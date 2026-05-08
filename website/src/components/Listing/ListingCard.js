import React from 'react';
import Link from '@docusaurus/Link';

export default function ListingCard({ icon, title, url, description, external }) {
  const Component = external ? 'a' : Link;
  const linkProps = external
    ? { href: url, target: '_blank', rel: 'noopener noreferrer' }
    : { to: url };

  return (
    <Component className="listing-card" {...linkProps}>
      {icon ? <div className="listing-card__icon">{icon}</div> : null}
      <h3 className="listing-card__title">{title}</h3>
      <p className="listing-card__description">{description}</p>
      <svg
        className="listing-card__arrow"
        width="14"
        height="14"
        viewBox="0 0 14 14"
        fill="none"
        stroke="currentColor"
        strokeWidth="1.8"
        strokeLinecap="round"
        strokeLinejoin="round"
      >
        <path d="M3 7h8M7 3l4 4-4 4" />
      </svg>
    </Component>
  );
}
