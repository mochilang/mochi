# C Algorithms Transpiler Output

This directory stores C code generated from Mochi programs in `tests/github/TheAlgorithms/Mochi`. Each file is compiled and executed during tests. Successful runs keep the generated `.c` source along with a matching `.out` file. Failures are recorded in `.error` files when tests run with `-update`.

Checklist of programs that currently transpile and run (46/50) - Last updated 2025-08-26 01:00 +0700:
| Index | Name | Status | Duration | Memory |
| ---: | --- | :---: | ---: | ---: |
| 719 | networking_flow/ford_fulkerson | ✓ | 506us | 1.5 MB |
| 720 | networking_flow/minimum_cut | ✓ | 511us | 1.6 MB |
| 721 | neural_network/activation_functions/binary_step | ✓ | 565us | 1.5 MB |
| 722 | neural_network/activation_functions/exponential_linear_unit | ✓ | 1.06ms | 1.5 MB |
| 723 | neural_network/activation_functions/gaussian_error_linear_unit | ✓ | 589us | 1.7 MB |
| 724 | neural_network/activation_functions/leaky_rectified_linear_unit | ✓ | 599us | 1.7 MB |
| 725 | neural_network/activation_functions/mish | ✓ | 652us | 2.1 MB |
| 726 | neural_network/activation_functions/rectified_linear_unit | ✓ | 457us | 1.6 MB |
| 727 | neural_network/activation_functions/scaled_exponential_linear_unit | ✓ | 496us | 1.9 MB |
| 728 | neural_network/activation_functions/soboleva_modified_hyperbolic_tangent | ✓ | 806us | 2.0 MB |
| 729 | neural_network/activation_functions/softplus | ✓ | 722us | 2.1 MB |
| 730 | neural_network/activation_functions/squareplus | ✓ | 568us | 1.5 MB |
| 731 | neural_network/activation_functions/swish | ✓ | 526us | 1.5 MB |
| 732 | neural_network/back_propagation_neural_network | ✓ | 275.14ms | 30.4 MB |
| 733 | neural_network/convolution_neural_network |  |  |  |
| 734 | neural_network/input_data | ✓ | 612us | 1.5 MB |
| 735 | neural_network/simple_neural_network | ✓ | 122.60ms | 1.6 MB |
| 736 | neural_network/two_hidden_layers_neural_network |  |  |  |
| 737 | other/activity_selection | ✓ | 778us | 1.6 MB |
| 738 | other/alternative_list_arrange | ✓ | 994us | 1.5 MB |
| 739 | other/bankers_algorithm | ✓ | 431us | 1.4 MB |
| 740 | other/davis_putnam_logemann_loveland | ✓ | 803us | 1.5 MB |
| 741 | other/doomsday | ✓ | 234us | 1.4 MB |
| 742 | other/fischer_yates_shuffle | ✓ | 254us | 1.5 MB |
| 743 | other/gauss_easter | ✓ | 500us | 1.8 MB |
| 744 | other/greedy | ✓ | 623us | 1.7 MB |
| 745 | other/guess_the_number_search | ✓ | 815us | 1.6 MB |
| 746 | other/h_index | ✓ | 373us | 1.4 MB |
| 747 | other/least_recently_used | ✓ | 751us | 1.6 MB |
| 748 | other/lfu_cache | ✓ | 281us | 1.6 MB |
| 749 | other/linear_congruential_generator | ✓ | 569us | 1.5 MB |
| 750 | other/lru_cache | ✓ | 639us | 1.4 MB |
| 751 | other/magicdiamondpattern | ✓ | 236us | 1.4 MB |
| 752 | other/majority_vote_algorithm | ✓ | 324us | 1.6 MB |
| 753 | other/maximum_subsequence | ✓ | 555us | 1.5 MB |
| 754 | other/nested_brackets | ✓ | 280us | 1.5 MB |
| 755 | other/number_container_system | ✓ | 278us | 1.5 MB |
| 756 | other/quine | ✓ | 198us | 1.5 MB |
| 757 | other/scoring_algorithm | ✓ | 903us | 1.7 MB |
| 758 | other/sdes |  |  |  |
| 759 | other/tower_of_hanoi | ✓ | 301us | 1.4 MB |
| 760 | other/word_search | ✓ | 252us | 1.7 MB |
| 761 | physics/altitude_pressure | ✓ | 700us | 2.2 MB |
| 762 | physics/archimedes_principle_of_buoyant_force | ✓ |  |  |
| 763 | physics/basic_orbital_capture |  |  |  |
| 764 | physics/casimir_effect | ✓ | 542us | 1.7 MB |
| 765 | physics/center_of_mass | ✓ | 324us | 1.5 MB |
| 766 | physics/centripetal_force | ✓ | 370us | 1.6 MB |
| 767 | physics/coulombs_law | ✓ | 260us | 1.6 MB |
| 768 | physics/doppler_frequency | ✓ | 669us | 1.6 MB |
