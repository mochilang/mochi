# COBOL Compiler Tasks

## Recent Enhancements
- 2025-07-13 05:00 - Variable initialization expressions are compiled after the WORKING-STORAGE section so `if` expressions work.
- 2025-07-13 05:01 - Checklist in machine README updated; nested if outputs regenerated.
- 2025-07-13 05:25 - Added interpreter-based constant evaluation to handle simple dataset queries.

## Remaining Work
- [ ] Support dataset queries (`group by`, joins) needed for TPCH programs such as `q1.mochi`.
- [ ] Generate helper routines for lists and maps to better match hand written examples.
- [ ] Add automated tests running the GNU COBOL compiler.
- [ ] Finish group by implementation for TPCH Q1.
