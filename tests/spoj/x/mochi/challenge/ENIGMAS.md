# ENIGMAS - Enigma Machine Simulation

https://www.spoj.com/problems/ENIGMAS/

Simulate the 3-rotor M3 Enigma machine (Rotors I-V, Reflector B).

## Approach

For each character: apply plugboard swap, step rotors (with double-step anomaly:
middle rotor steps both when triggered by right rotor notch AND when at its own
notch), pass signal forward through R→M→L rotors, reflect, pass backward through
L→M→R, apply plugboard again. Rotor signal formula: offset = (sig + pos - ring
+ 52) % 26, look up wiring, output = (wired - pos + ring + 52) % 26. Preserve
original quintuple grouping from the input message.
