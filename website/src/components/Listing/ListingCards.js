import React from 'react';

export default function ListingCards({ children, columns }) {
  const style = columns
    ? { gridTemplateColumns: `repeat(${columns}, minmax(0, 1fr))` }
    : undefined;
  return (
    <div className="listing-cards" style={style}>
      {children}
    </div>
  );
}
