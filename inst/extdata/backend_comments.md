# Done tasks

---

### Done ✅

> You should add formatted comments for the "jump menu" in SLiMgui.
>
> [...]
>
> You could also annotate the individual script blocks with structured comments
>
> [...]

---

### Done ✅

> You have elements in the script that I guess get replaced by your R script, like "{{seed}}".  For those, maybe it would be nice to have a comment to the right saying what type they are? [...] Of course, besides their type it would also be useful to say what they mean [...].

---

### Done ✅ (and more)

> [...] It'd be nice if your script was syntax-compliant, so maybe you can look for a different way of doing this templating?  One possibility would be to just use a placeholder that already looks like a symbol, like X_X_SEED instead of {{seed}}, or something. [...]

I have now changed the backend from an Eidos/SLiM-noncompliant code (which used those `{{var}}` substitution tags) to normal SLiM command-line parameters. I had a specific idea for the R templating (it was supposed to power those custom user-defined SLiM "modules" I mentioned and make them easy to work with in R) but it's an overkill in this case.

There are two important benefits of his change:

1. Once a user `compile()`s a slendr model, a dedicated SLiM script will be saved in the model directory and can be run from the command-line as any other SLiM script. I.e., the `slim()` runner function in slendr now passes the parameters via CLI instead of substituting the template `{{var}}` variables. The main benefit is increased flexibility. The models can be defined and compiled on one machine (in R), and then transfered to any computer with SLiM for execution. I know some institutions have very restricted cluster capabilities -- this is one way to get around that.

Before this change, the script was saved as one "monolithic" unit with all parameter values via the `{{var}}` substitution, hardcoded by the parameters given to the `slim()` R function.

2. The `compile()` function now accepts an optional argument specifying where it should look for the backend SLiM script. By default, it's the script bundled under `inst/extdata/script.slim` in the R package. But it could be also a slightly modified version of this script, with bits of added functionality (custom script blocks, etc.). As long as the core of the script remains the same, the (advanced) user can extend the functionality however they need, and still maintain the reproducibility of having the entire analysis/simulation workflow done from R. If the user does this, there will be a gentle warning that this is an experimental feature (and that the slendr model handling SLiM code mustn't change).

This solves my issue with those optional "modules". There was always an option to modify the monolithic SLiM script compiled by the package, but that would break reproducibility because there wasn't a way to include it into the slendr workflow. This is not a problem anymore.

---

### Done ✅

> You seem to be using two-space indents, I'd suggest tabs?

I hope this won't ruin our friendship, but I'm a card-carrying member of the spaces-but-not-tabs camp. 😬 I do admit that the two-space indents come from an R style guide which doesn't fit SLiM code (or non-R code in general). I changed the code to four-space indents. Hopefully a reasonable compromise.

---

### ✅ Done

> "g1.mutationTypes[mut_types]" seems odd, shouldn't that just be "g1.mutationTypes"?  And is there a need to set their stacking policy stuff?  They will never stack anyway, right?  How could they, if new mutations never occur?

You're right. I think this was a result of some of my very early misunderstanding of how mutation types should be handled here (going back all the way to before *slendr* was even an R package).

---

### Done ✅

> You do "N = event.getValue("N");", so N is a variable. [...] how about "pop_size" or some such?

Given that UPPER_CASE_CONSTANTS are now basically all command-line arguments, I'm actually thinking about making them lower-case. I personally don't see much of a reason to call the default slendr SLiM script outside of R, but having to type out `-d UPPER_CASE_CLI_ARGUMENT=123` somehow bothers me a bit. A matter of esthetics though.

---

### Done ✅

> add_markers() checks if (TRACK_ANCESTRY) and does nothing if it's false.  Seems weird to me; more normal would be to put the if (TRACK_ANCESTRY) outside the function, in the caller not the function.  This is because the function name suggests that the function simply "adds markers" [...] A nit, but this sort of thing can cause confusion.  Functions/methods should do what their name says they do, to the extent possible.  Sorry for being pedantic, I feel a little guilty about this comment.  :->

No reason to feel guilty. As a fellow code pedant, I appreciate this comment. 👍 In my quasi-semi-defense, I have no idea why this condition was even there. I think it must be a left-over of a pre-table/Dictionary iteration of the backend which probably did something slightly more in this function.

---

### Done ✅

@peterlharp Does this look like a reasonable way to do #61 and #62? I modified the format of the metadata slightly (moved `"description"` one level "higher" in the hierarchy directly under the `"slendr"` key) and simplified it only to what's used by slendr in its current state. It appears that this metadata wouldn't currently be used for anything else except slendr itself, but it would be quite easy to expand/modify the format should other spatial tools pop up at some point.

---

### Done ✅

> slim_time(), yikes.  That's a lot of complicated code just to translate times.  Would it be possible to do a different approach to this?  You could maybe (a) write out the translated times in the first place so they don't need to be shifted at runtime, or (b) do the shift by simply adding BURNIN_LENGTH when you *use* a time value that you just got from a table?  Don't know if any of this is practical, but this sort of complex dictionary-munging is definitely an obstacle to understanding how the script works.

~~Three paragraphs of lame excuses and justification for why I can't or don't want to address this right now. 🤦‍♀️~~

Wow, this was a brilliant point. It took me a whole week sitting on this, while I was implementing your other comments but when I actually found a good way to implement your suggestions (option (b)). This made the code *so much* cleaner. Thank you!

---

### Done ✅

> original_time(), I'm not really sure what it does.  In what units is "current generation time", in what units is "the original units", why are they different, etc.?  The comment on this function needs to be improved.

Same as the above. It was surprisingly painful to do this, but I'm so glad I did it. Lots of code was removed by doing this -- the best kind of refactoring. :)

---

# Tasks in progress

---

### In-progress ⏳

> I wonder whether any of the output you do should be done with LogFile?  Or if you might want to add the option for additional output of runtime stats/metrics using LogFile?

That's something Peter also suggested at some point but I have never managed to get to this. Now that we are close to the final feature set, this is definitely a quality of life improvement worth revisiting. I will read the appropriate section of the manual and see what I can do (in a separate PR, probably).

---

### In-progress ⏳

> set_coordinates() could be made vastly more efficient by vectorizing it, but if it's not a bottleneck don't worry about it, because the way to vectorize it is a little bit gross (see https://github.com/MesserLab/SLiM-Extras/blob/master/models/Recipe_15.10_OnLand.slim)

Ah, this is clever. Also, yet another case where I missed a clear vectorization possibility. I actually think it makes sense to optimize the code as much as possible. I will do some profiling (I've never actually used the SLiM profiler before 😬) first and will tackle this in a separate PR. I might also just wait and see if there are problems down the line, as this would be simply an internal change, not something affecting potential slendr models in the wild.

---

> Note that "asInteger({{seq_length}})" is unnecessary unless the string you put in place of {{seq_length}} is not an integer (which seems like it would be a bug?).  In Eidos, unlike in R, 10 is an integer, not numeric, and in fact 5e5 is also an integer.  Ah, but 5.5e5 is a float, which is arguably a bug in Eidos; that should be an integer too.  So OK, maybe you need that call.  Just thinking out loud, and now I'm curious whether you guys agree that 5.5e5 being float is a bug in Eidos.  :->

My vote would be for 5e5 being an integer and not a float, if only because the "an integer-looking number is numeric but not integer by default" thing in R always bothers me a bit.

---

### Zoom call ☎️

> I see that you do "initializeMutationRate(0.0);".  So slendr simulations never have new mutations at all?  It looks like you define genomic element types only for ancestry inference, when tree sequence recording is turned off I suppose?  And I guess neutral mutations can be overlaid, with treeseq at least.

In my original plan for slendr, mutation rate was supposed to be a flexible parameter (I was aiming to have those user-defined SLiM "modules" for selection etc. which would include user-defined non-neutral mutation types). Now that this is off the table for the nearest future, I'm actually thinking that the default way to add mutations would be ne

This actually relates to my other point -- is there a reason (in the current *slendr* design) to support other output type than tree sequences? If tree sequences are to be the main output type (which I'm striving to make as easy to use from R as possible), do we even want to support something else than `initializeMutationRate(0.0)`? Thoughts @petrelharp @FerRacimo?

W.r.t. ancestry tracking mutations, it would be great if SLiM could track ancestry proportions without this. This was initially a hack I used to get an idea whether I implemented gene flow correctly, which I kept because it seemed like a useful thing to have.

---

### Zoom call ☎️

> I don't think I'm going to try to thoroughly grok your whole script; it's too complex and difficult to understand since it is table-driven etc.  That's nothing against your code or design, it looks very clean to me.  It just takes a lot of work to get deeply into a script like this without the help of the author.  :->  I'd be happy to have a zoom with you where we go over the script line by line, though; that would probably be useful for both of us.  Anyway, I'm going to limit myself to more superficial comments based on what I see just skimming your code without trying to deeply understand it.

I agree, a separate Zoom call where we could go through this would be great.

---

### Zoom call ☎️

> Your 2: fitness(NULL) callback would probably be considerably more efficient if it were rewritten to (a) not be a callback, (b) use fitnessScaling, and (c) be vectorized.  I don't know whether speed is an issue for your stuff or not.  If it is, you should do a profile in SLiMgui to see where your bottlenecks are, and then perhaps I can help you whittle them down.

- In several callbacks you do "if (!SPATIAL) return NULL;" or similar, to deactivate the callback.  That's inefficient because SLiM has to do all the setup/teardown for the callback, over and over, just to have it turn out to do nothing.  A better design would be to give these script blocks symbolic names, and then, once in your generation 1 setup event, do "if (!SPATIAL)" and remove the script blocks you don't want to ever run.  See "deregisterScriptBlock()".

- In your modifyChild() callback you do "return F;" if a location is generated that can't be used.  This amounts to "absorbing boundaries", which is probably not what you want; I'd suggest "reprising boundaries", which would loop until a legal position is generated.  If it is plausible that that could be an infinite loop, then you could set a maximum number of tries by using a for loop with a return on success, and return F if the for loop completes.

---

### Done ✅

> evaluate_interactions() does "if (SPATIAL) sim.interactionTypes.evaluate();".  The whole existence of this function seems unnecessary to me.  If SPATIAL is F, there will be no defined interaction types, right?  So then calling "sim.interactionTypes.evaluate();" would do nothing, because sim.interactionTypes would evaluate to a zero-length vector.  Am I missing something?

Can't honestly remember why I wrote a dedicated function to this. I guess I wasn't aware that calling a method on a zero-length vector object wouldn't crash. There might be cases where I'm doing strange things with interactions and/or their evaluations. Might be worth keeping this in mind during the next Zoom session.

---

### Zoom call ☎️

> calc_ancestry(), interesting.  I really wish we could do this with tree-sequence recording.

Me too! I noticed you discussed this with Peter in our email thread recently. This would be an *amazing* feature to have. Lots of human history reconstruction deals with estimating ancestry proportions coming from a set of populations into some target group of individuals and having this built in would be a huge help. I imagine it would speed things up quite a bit too.

---

### Zoom call ☎️

> Your functions to work with Dictionaries are interesting.  read_table() could instead use Dictionary's JSON-string constructor, if you wrote out your tables as JSON rather than .tsv.  But maybe being able to read a Dictionary from a .tsv is also useful functionality that ought to be in Eidos.  Not sure.  So far my focus has been on JSON because that's what Graham Gower seems to be interested in using.

The reason I went with simple plain text tables as the format for all model config files was that this made it much easier for me to debug and inspect issues just by looking at the tables (in shell or in R), even when they were very long. If there was a way to load tabular data as JSON/Dictionaries, that would make everything so much easier!

In principle, what I have works, except for the crucial point of not being able to guess the column types.

---

### Zoom call ☎️

> - filter(), also interesting.  Something like this could also conceivably be added to Eidos.
> 
> - num_rows(), ditto.  Possibly this idea of a Dictionary as a dataframe-like object ought to be more formalized.  Eidos could define a subclass of Dictionary named DataFrame that defined more of this sort of functionality.
> 
> - print_table(), ah, interesting.  Yes, the default print for Dictionary is not so readable when the Dictionary is, conceptually, a dataframe.  Again, if DataFrame were a subclass this could be handled in Eidos.

This would be all incredibly useful additions! I would be very happy if these wouldn't have to be implemented in the backend code. Anything that makes the script shorter is great and if these (or similar) functions would be built in and the user could look them up in the manual, even better.

---

### Zoom call ☎️

> convert_type(), eww.  :->  I'd like to better understand exactly why this is needed.  It might be that if you switched to JSON-serialized tables the need for this would disappear.

In a way, I'm happy to see that this awful piece of code is not just my least hated part of the codebase, but also yours. :)

Long story short, unless I completely missed something, the `read_table` function doesn't have a way to know which columns are of which type. It uses `readFile()` to read the whole file, converting it to a matrix via `strsplit()` call. But my attempts at doing something like `try to convert a column to a type integer, or a float, or a logical and if an error is caught, keep it as a string` failed. Maybe I missed something?

Again, saving and reading the data as a JSON would obviously help, but I'd rather not change the guts of the package to do this (lots of code currently assumes the data is saved in a plan text tabular form).

But related to the previous point -- is there any chance that a .tsv->Dictionary loading functionality could be added to Eidos? In terms of type guessing, Ivarious .tsv loading R packages simply try to read the first N elements of a column and then decide what the type should be (unless the user specifies the column types upfront).

Having a `can these values be converted to a given type, let me know` function would make the implementation of this much nicer. Then again, having a dedicated table-loading function would be even better.

---

> Hmm.  Well, my initial reaction is that this whole idea of "modules" etc. sounds like a large increase in complexity for slendr, and I wonder whether it will really make people happy.  There are a million different ways that people might want to extend a slendr model, and I think you might drive yourself crazy trying to support all of them.  Let slendr do what it does; it does it very well.  If people want to go beyond that, maybe it would be best for them to generate the slendr SLiM script, and then start hand-modifying it to go beyond slendr's capabilities.  Otherwise you end up trying to re-implement in slendr everything that SLiM can do, and that will never end.  If, on the other hand, you say "oh, but that kind of open-ended kitchen-sink thing isn't really what I want to do, I just want to add support for very simple selection models", then maybe a more directed fix that doesn't involve "modules" etc. would be better?  But I'm not sure, that's just my gut reaction.

We already talked about this in the last meeting but it's worth repeating here: thank you for your perspective. What you said makes perfect sense and I'm glad you said it. I should focus to make slendr to what it currently does best. If the user decides they want to
