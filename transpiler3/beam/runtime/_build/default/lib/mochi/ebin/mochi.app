{application,mochi,
             [{description,"Mochi runtime for BEAM"},
              {vsn,"0.1.0"},
              {modules,[mochi_app,mochi_atoms,mochi_sup]},
              {registered,[]},
              {applications,[kernel,stdlib,sasl]},
              {mod,{mochi_app,[]}}]}.
