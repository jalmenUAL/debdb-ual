# Debugging of DBpedia SPARQL Queries

**University of Almería, 2025**

This repository contains the debugger for **DBpedia SPARQL queries**. The debugger aligns SPARQL queries with **expected and unexpected answers** by converting queries into rules and identifying query rewrites that satisfy these criteria. It uses **constraint solving** to determine filter conditions consistent with expected and unexpected results.

---

## Features

- **SWI-Prolog Implementation:** Core debugger implemented in Prolog (`dbprex.pl`).
- **Web Tool:** Interactive web interface available [here](https://github.com/jalmenUAL/debdb).
- **Web Tool Page:** Access the web tool online at [http://balmis.ual.es](http://balmis.ual.es).

---

## Debugging Process

The debugger workflow can be summarized as follows:

```mermaid
flowchart TD
    A[Start: Weak Rule Construction] --> B[Select a weak rule wr]
    B --> C[Encode three SPARQL queries for wr, expected (e1..en), unexpected (u1..um)]
    C --> D[Execute 1st conjunctive query (all expected)]
    D -->|Success| E[Constraint solver: check satisfiability of bindings]
    D -->|Failure| F[Try another weak rule]
    E --> G[Execute 2nd disjunctive query (some unexpected)]
    G -->|Failure| H[wr accepted as valid query]
    G -->|Success| I[Execute 3rd conjunctive query (all unexpected)]
    I --> J[Constraint solver: check negation satisfiable]
    J -->|Yes| H
    J -->|No| F
    H --> K[End / Rule found]
    F --> B
# Debugging of DBpedia SPARQL Queries

**University of Almería, 2025**

This repository contains the debugger for **DBpedia SPARQL queries**. The debugger aligns SPARQL queries with **expected and unexpected answers** by converting queries into rules and identifying query rewrites that satisfy these criteria. It uses **constraint solving** to determine filter conditions consistent with expected and unexpected results.

---

## Features

- **SWI-Prolog Implementation:** Core debugger implemented in Prolog (`dbprex.pl`).
- **Web Tool:** Interactive web interface available [here](https://github.com/jalmenUAL/debdb).
- **Web Tool Page:** Access the web tool online at [http://balmis.ual.es](http://balmis.ual.es).

---

## Debugging Process

The debugger workflow can be summarized as follows:

```mermaid
flowchart TD
    A[Start: Weak Rule Construction] --> B[Select a weak rule wr]
    B --> C[Encode three SPARQL queries for wr, expected (e1..en), unexpected (u1..um)]
    C --> D[Execute 1st conjunctive query (all expected)]
    D -->|Success| E[Constraint solver: check satisfiability of bindings]
    D -->|Failure| F[Try another weak rule]
    E --> G[Execute 2nd disjunctive query (some unexpected)]
    G -->|Failure| H[wr accepted as valid query]
    G -->|Success| I[Execute 3rd conjunctive query (all unexpected)]
    I --> J[Constraint solver: check negation satisfiable]
    J -->|Yes| H
    J -->|No| F
    H --> K[End / Rule found]
    F --> B
Step-by-Step Explanation

Weak Rule Construction:

Build a set of weak rules Weak(r) and apply them to sets of expected (e1,...,en) and unexpected (u1,...,um) answers.

SPARQL Encoding:

Each weak rule wr is encoded into three SPARQL queries:

Conjunctive query for expected answers: Are all e1,...,en covered by wr?

Disjunctive query for unexpected answers: Does some u1,...,um match wr?

Conjunctive query for unexpected answers: Are all u1,...,um answers of wr?

Execution & Constraint Solving:

First query: If success, obtain variable bindings and check satisfiability.

Second query: If failure → accept rule; if success → execute third query.

Third query: Check if negation of bindings is satisfiable. If yes → accept rule; else → try next weak rule.

Iteration:

Repeat until a valid weak rule is found or all rules are tested.

Usage

Run the Prolog debugger:

swipl dbprex.pl


Provide inputs in the console:

SPARQL query to debug

Sets of expected and unexpected answers

Optional: Use the web tool for GUI-based debugging.

References

DBpedia

SWI-Prolog: https://www.swi-prolog.org

License

This project is released under the MIT License.
