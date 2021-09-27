# Postcode finder

Elegantly written in Elm and Create Elm App - CEA

This site is hosted on Netlify and can be found @ **[jolly-dijkstra-5d82c8.netlify.app]**

This project took around two hours.

## Assumptions made

- A postcode is between 5 and 7 characters and includes no symbols.

  > Q: Guidance on recording valid postcodes
  > A: The postcode is a combination of between five and seven letters/numbers, which define four different levels of geographic unit. It is part of a coding system created and used by the Royal Mail across the United Kingdom for sorting mail.

- Any text values sent are sanitised on the server

## Future features (in no particular order)

This app could include a number of features, of differing value to the maintainer, and end-user.

- CI
- Accessibility
- Enter key also triggers search
- More fields in search form (lat/lon)
- Analytics
- Advertising & SEO
- Language support
- Dark mode
- Elastic-search

## With more time

Mentioned above are features not implemented. In addition to these features there are a couple of ways this application could be improved including:

- Client-side validation/ sanitisation of form fields.
- Codebase to be refactored into reusable low-level components.
- Tests to check button click response; and character limit works in the input field.
