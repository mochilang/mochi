# SPOJ ZRELY1 - Logic Circuit Reliability

https://www.spoj.com/problems/ZRELY1/

Given a logic circuit, output a circuit that maximises COF (correct-output fraction under random gate failures). The baseline score passes the input circuit through unchanged.

## Approach

Pass-through strategy: read each test case's circuit and echo its gates back unchanged. This achieves the baseline COF score (worth 276 points). Input includes Z test cases; each specifies a redundancy budget K, 6 gate-type area/failure parameters, I inputs, O outputs, and N gates. Output is the gate count followed by each gate line.
