import React from 'react';
import Link from '@docusaurus/Link';
import ListingCards from '@site/src/components/Listing/ListingCards';
import ListingCard from '@site/src/components/Listing/ListingCard';

export default function DocsLanding({ title, lede, primaryCards, sectionGroups }) {
  return (
    <div className="docs-landing">
      <header className="docs-landing__header">
        <h1 className="docs-landing__title">{title}</h1>
        <p className="docs-landing__lede">{lede}</p>
      </header>

      {primaryCards && primaryCards.length > 0 ? (
        <section className="docs-landing__section">
          <ListingCards>
            {primaryCards.map((card) => (
              <ListingCard key={card.url} {...card} />
            ))}
          </ListingCards>
        </section>
      ) : null}

      {sectionGroups && sectionGroups.map((group) => (
        <section key={group.title} className="docs-landing__section">
          <h2 className="docs-landing__group-title">{group.title}</h2>
          {group.lede ? <p className="docs-landing__group-lede">{group.lede}</p> : null}
          <ListingCards>
            {group.cards.map((card) => (
              <ListingCard key={card.url} {...card} />
            ))}
          </ListingCards>
        </section>
      ))}
    </div>
  );
}
