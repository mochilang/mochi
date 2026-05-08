import React from 'react';

export default function Tooltip({ parent, content }) {
  return (
    <span className="mochi-tooltip">
      {parent}
      <span className="mochi-tooltip__content">{content}</span>
    </span>
  );
}
