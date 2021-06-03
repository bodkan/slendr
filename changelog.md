# Changes since the last alpha version:

- Geographic landscapes and coordinate systems are no longer tied to the real Earth geography and can be fully abstract (including completely arbitrary shapes of the "continents", geographic barriers etc.).

- New spatial set operations: `combine()`, `overlap()`, `subtract()`. Users can use these do define more complex larger population boundaries and geographic regions from smaller ones.

- New [R shiny](http://shiny.rstudio.com)-based interactive exploration "app": `explore()`. The user can provide a compiled model object (i.e. formal configuration of spatial population dynamics and gene flow events) and the package will spawn a browser app with a time slider. This can be used to inspect the model spatial dynamics interactively before they are sent over to the SLiM side.

- Spatial interaction distances (translated to `maxDistance` on the SLiM side) as well as offspring distances from parents (i.e. the standard deviations of their normal distributions) can now be specified for each population individually. The users can still specify default values for all populations (or just those which did not have their own dedicated parameter values) in the main `slim()` call.

- It is now possible to define models in forward or backwards direction, depending on what is more convenient for the user and the scenario that is being modeled. The direction of time is automatically detected and translated to SLiM's units of generations in the forward direction.

- The explicit (and a little redundant) `sim_length` parameter was finally removed.


- Any simulation can now have multiple "ancestral" populations (i.e. populations without an immediate ancestor created by `sim.addSubpop()`). Until now, all populations had to trace their ancestry to a single ancestor (a leftover of a hard-coded requirement from the very first version of my code).

- Renamed `admixture()` to `geneflow()`.

- The `plot.slendr` generic function 

## In progress

Most likely the last things before making the first public release:

- A straightforward way to specify when and how many individuals should be remembered for the tree sequence output or location tracking, and more generally, provide a way to schedule custom output events (user-defined SLiM functions) on the R side, that will be triggered at the scheduled times on the SLiM side.

- Allow simulations of marine species (this simply requires adding a single argument to the `population()` call which will invert the intersection with landscape objects).

