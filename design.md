# Calq Design Document

Calq has the following concepts:
1. `Axis`: An independent measurement axis (e.g. length, time, currency, etc.).
2. `Unit`: A unit of measurement along an axis (e.g. meters, seconds, USD, etc.).
3. Each axis has a _cannonical_ Unit (e.g. the cannonical Unit for length is meters).
4. Units also have `Alias`es. E.g. "feet" is an alias for "ft".
5. Units are variables that are defined in terms of other Units. This creates a DAG
   where the leaf nodes are cannonical units which have no definitions.
