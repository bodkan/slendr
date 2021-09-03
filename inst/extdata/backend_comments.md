### ✅ Done

> You should add formatted comments for the "jump menu" in SLiMgui.  For example, you have a big comment:
> ```
> //////////////////////////////////////////////////////////////////////
> // Scheduled script blocks
> //////////////////////////////////////////////////////////////////////
> ```
> If you instead write that as:
> ```
> ///
> /// Scheduled script blocks
> ///
> ```
> Then you'll get a jump menu that has a bold "scheduled script blocks" header with dividers above and below; very nice.  Do the same for your comment "Simulation utility functions".  The jump menu not only makes it easier to navigate around in a big file like this, but also makes it easy to approach an unfamiliar script by providing a top-level outline of what's going on.
> 
> You could also annotate the individual script blocks with structured comments like this.  For example, if you rewrite your comment on 1 early() as:
> 
> 1 early() // Schedule script block events
> {
> 
> then it displays in the Jump menu with the comment "Schedule script block events" after "1 early()", in the same menu item.  Doing this for most/all of your events and callbacks would be very useful, IMHO.  You can even use emoji if you want to, which seems silly at first, but actually makes it very visual - the eye then doesn't have to read, but can look for the "rocket ship" for simulation start, or the "baby" for reproduction, or whatever the case may be.  Probably not an emoji in every comment, but used sparingly for the most important spots, they can be useful.  I am increasingly enamored with this sort of annotation (not just emoji, but the jump menu annotations in general).


Brilliant, I had no idea about this feature. 👍

---

> You have elements in the script that I guess get replaced by your R script, like "{{seed}}".  For those, maybe it would be nice to have a comment to the right saying what type they are?  It's just a bit hard to read the script when the very first thing it does is define a whole bunch of constants that, AFAIK, could be of any type and have any sort of value.  A comment hint would thus be helpful.  Of course, besides their type it would also be useful to say what they mean, very briefly; i.e., is GENERATION_TIME the total length of the simulation, or a scaling factor that translates between model ticks and biological generations, or what?

This is an excellent point. I have now changed to backend from an Eidos/SLiM-noncompliant code (which used those `{{var}}` R templating substitution markers) to normal SLiM command-line parameters. I had originaly a specific idea for the R templating (it was supposed to power those custom user-defined SLiM "modules") but it's an overkill in this case.

---

### ✅ Done

> You seem to be using two-space indents, I'd suggest tabs?  Note that SLiMgui can prettyprint your code for you, either shallowly (click the button) or deeply (option-click the button).

I hope this won't ruin our friendship, but I'm a card-carrying member of the spaces-and-not-tabs camp. 😬 However, I do admit that the two-space indent style comes from an R style guide which perhaps doesn't make sense to use for non-R code (it's just a default that RStudio has that I just stuck with). My eyes are already used to these shallow indents, but I changed these to four-space indents. Hopefully a reasonable compromise.

---

### ⏳ In-progress

> Ah, so, I just tried to prettyprint your script in SLiMgui, since the tiny indents were bothering me.  :->  It gave an error, "unexpected token '{'", because of your {{seed}} type placeholders, which violate Eidos syntax.  It'd be nice if your script was syntax-compliant, so maybe you can look for a different way of doing this templating?  One possibility would be to just use a placeholder that already looks like a symbol, like X_X_SEED instead of {{seed}}, or something.  Another possibility would be to use a string, like "{{seed}}" including the quotes, and replace not just the {{seed}} part but the whole "{{seed}}" placeholder.  Etc.  Anything so the script parses as compliant Eidos code; without that, SLiMgui has one hand tied behind its back, and I pretty much always work in SLiMgui.

As explained above, I moved towards specifying the arguments on the command-line.

---

### 💡

> Note that "asInteger({{seq_length}})" is unnecessary unless the string you put in place of {{seq_length}} is not an integer (which seems like it would be a bug?).  In Eidos, unlike in R, 10 is an integer, not numeric, and in fact 5e5 is also an integer.  Ah, but 5.5e5 is a float, which is arguably a bug in Eidos; that should be an integer too.  So OK, maybe you need that call.  Just thinking out loud, and now I'm curious whether you guys agree that 5.5e5 being float is a bug in Eidos.  :->

My vote would be for 5e5 being an integer, if only because the "numeric but not integer" thing in R bothers me a bit. :)

---

### ☎️ Zoom call?

> I see that you do "initializeMutationRate(0.0);".  So slendr simulations never have new mutations at all?  It looks like you define genomic element types only for ancestry inference, when tree sequence recording is turned off I suppose?  And I guess neutral mutations can be overlaid, with treeseq at least.

Calling `initializeMutationRate(0.0)` is a remnant of me originally not knowing what is it all that *slendr* would be doing in the final version. The original plan (which included those user-defined SLiM "modules") was that this would be flexible, including specifying user-defined non-neutral mutation types. Now that we agreed that the optional selection (and other) modules will not be heavily emphasised in the first version, perhaps I could add `mutation_rate` as an optional argument of the `slim()` functions.

This actually relates to my other point -- is there a reason (in the current *slendr* design) to support other output type than tree sequences? If tree sequences are to be the main output type (which I'm striving to make as easy to use from R as possible), do we even want to support something else than `initializeMutationRate(0.0)`? Thoughts @petrelharp @FerRacimo?

W.r.t. ancestry tracking mutations, it would be great if SLiM could track ancestry proportions without this. This was initially a hack I used to get an idea whether I implemented gene flow correctly, which I kept because it seemed like a useful thing to have.

---

### ✅ Done

> "g1.mutationTypes[mut_types]" seems odd, shouldn't that just be "g1.mutationTypes"?  And is there a need to set their stacking policy stuff?  They will never stack anyway, right?  How could they, if new mutations never occur?

This sounds correct. I think this might be a result of some of my very early misunderstanding of how mutation types should be handled here (going back all the way to before *slendr* was even an R package).

---

### ✅

> I don't think I'm going to try to thoroughly grok your whole script; it's too complex and difficult to understand since it is table-driven etc.  That's nothing against your code or design, it looks very clean to me.  It just takes a lot of work to get deeply into a script like this without the help of the author.  :->  I'd be happy to have a zoom with you where we go over the script line by line, though; that would probably be useful for both of us.  Anyway, I'm going to limit myself to more superficial comments based on what I see just skimming your code without trying to deeply understand it.

I agree, a separate Zoom call where we could go through this would be great. Let's do this some time after the tskit meeting.

I admit that the script grew much larger than I expected. I'm very happy with the Dictionary/table-design because it made the code *much* easier for me to write to support all the possible *slendr* defined models regardless how crazy the model specification is (the alternative would be to generate lots of auto-generated SLiM code). But it does turn the backend kind of into its own thing (which is honestly a huge 👏 to SLiM/Eidos).

---

### Done ✅

> You do "N = event.getValue("N");", so N is a variable.  I know that's the standard popgen symbol of course, but I try to reserve all-caps symbols for defined constants to avoid confusion.  I first saw "N" being slightly later, and thought "oh, he defined a constant N, that's surprising", and only when I traced it back did I realize it's a variable.  Since "n" would not be great, how about "pop_size" or some such?

---

### Zoom ☎️

> Your 2: fitness(NULL) callback would probably be considerably more efficient if it were rewritten to (a) not be a callback, (b) use fitnessScaling, and (c) be vectorized.  I don't know whether speed is an issue for your stuff or not.  If it is, you should do a profile in SLiMgui to see where your bottlenecks are, and then perhaps I can help you whittle them down.

- In several callbacks you do "if (!SPATIAL) return NULL;" or similar, to deactivate the callback.  That's inefficient because SLiM has to do all the setup/teardown for the callback, over and over, just to have it turn out to do nothing.  A better design would be to give these script blocks symbolic names, and then, once in your generation 1 setup event, do "if (!SPATIAL)" and remove the script blocks you don't want to ever run.  See "deregisterScriptBlock()".

- In your modifyChild() callback you do "return F;" if a location is generated that can't be used.  This amounts to "absorbing boundaries", which is probably not what you want; I'd suggest "reprising boundaries", which would loop until a legal position is generated.  If it is plausible that that could be an infinite loop, then you could set a maximum number of tries by using a for loop with a return on success, and return F if the for loop completes.

---

### ✅

> evaluate_interactions() does "if (SPATIAL) sim.interactionTypes.evaluate();".  The whole existence of this function seems unnecessary to me.  If SPATIAL is F, there will be no defined interaction types, right?  So then calling "sim.interactionTypes.evaluate();" would do nothing, because sim.interactionTypes would evaluate to a zero-length vector.  Am I missing something?

No, you're not missing anything. It's I who missed this obvious logical implication. :) Speaking about missing obvious things, I also didn't realize I could have simply called `sim.interactionTypes.evaluate()` anyway. For someone who's primary reason to love R is its ever-present vectorization I sure am not using it to it's full potential in SLiM...

--- ✅

> add_markers() checks if (TRACK_ANCESTRY) and does nothing if it's false.  Seems weird to me; more normal would be to put the if (TRACK_ANCESTRY) outside the function, in the caller not the function.  This is because the function name suggests that the function simply "adds markers"; if it does what you have it doing, it should be named "add_markers_if_tracking_ancestry" or some such.  A nit, but this sort of thing can cause confusion.  Functions/methods should do what their name says they do, to the extent possible.  Sorry for being pedantic, I feel a little guilty about this comment.  :->

No reason to feel guilty. As a fellow nitpicker and code pedant, I appreciate this comment. 👍 In my quasi-semi-defense, I have no idea why this is even there. 😅 I think it must be a left-over of a pre-table/Dictionary iteration of the backend which probably did something slightly more in this function. Thanks for bringing this to my attention. 

---

### ⏳ in-progress

> set_coordinates() could be made vastly more efficient by vectorizing it, but if it's not a bottleneck don't worry about it, because the way to vectorize it is a little bit gross (see https://github.com/MesserLab/SLiM-Extras/blob/master/models/Recipe_15.10_OnLand.slim)

Ah, this is clever. Also, yet another case where I missed a clear vectorization possibility. I actually think it makes sense to optimize the code as much as possible. I will do some profiling (I've never actually used the SLiM profiler before 😬) and update the PR accordingly based on what I find.

---

### ⏳

> I wonder whether any of the output you do should be done with LogFile?  Or if you might want to add the option for additional output of runtime stats/metrics using LogFile?

Absolutely. That's something Peter also suggested at some point but I have never managed to get to this. I think I even opened a Github issue at some point but closed it as there were so many other new features to implement. Now that we are closer to a final feature set, this is definitely a quality of life improvement worth revisiting.

---

### ☎️ Zoom topic?

> calc_ancestry(), interesting.  I really wish we could do this with tree-sequence recording.

Me too! I noticed you discussed this with Peter in our email thread recently. Not sure if I can help with implementing this on the SLiM/tskit level, but this would be an *amazing* feature to have. Lots of human history reconstruction deals with estimating ancestry proportions coming from a set of populations into some target group of individuals and having this built in would be a huge help. I imagine it would speed things up quite a bit too.

---

### ☎️ Zoom topic?

> Your functions to work with Dictionaries are interesting.  read_table() could instead use Dictionary's JSON-string constructor, if you wrote out your tables as JSON rather than .tsv.  But maybe being able to read a Dictionary from a .tsv is also useful functionality that ought to be in Eidos.  Not sure.  So far my focus has been on JSON because that's what Graham Gower seems to be interested in using.

So, in hindsight, I wish I had figured out a way to save all configuration files to JSON from the get go. It's just that tables were much easier to work with and debug. All slendr tables are storing time-series configuration data, one row per time point, making it really easy to debug things just by looking at the tables, even when they were very long. If there was a way to load tabular data as JSON/Dictionaries, that would make the backend script much shorter and easier to debug... more on that later. :)

---

### ☎️ Zoom topic?

> - filter(), also interesting.  Something like this could also conceivably be added to Eidos.
> 
> - num_rows(), ditto.  Possibly this idea of a Dictionary as a dataframe-like object ought to be more formalized.  Eidos could define a subclass of Dictionary named DataFrame that defined more of this sort of functionality.
> 
> - print_table(), ah, interesting.  Yes, the default print for Dictionary is not so readable when the Dictionary is, conceptually, a dataframe.  Again, if DataFrame were a subclass this could be handled in Eidos.

This would be all incredibly useful additions! I would be very happy if these wouldn't have to be implemented in the backend code. Anything that makes the script shorter is great and if these (or similar) functions would be built in and the user could look them up in the manual, even better.

---

### ✅ Done

> iter() strikes me as maybe user-defined-function overkill.  What it does is trivial, and I think the code would be clearer if you just replaced calls to iter(d) with seqLen(num_rows(d)).  I was wondering, as I read your code, what this "iter" thing was and why you were using it.

Hmm, good point, I think. Again, I think this functions originates from the first Dictionary-based backend reimplementation. I think that I originally tried to have something aking to Python's `iter` which would, in this case, return rows of the Dictionary one by one? I agree with your assessment though and replaced `iter` with your suggestion.

---

### ☎️ Zoom topic?

> convert_type(), eww.  :->  I'd like to better understand exactly why this is needed.  It might be that if you switched to JSON-serialized tables the need for this would disappear.

In a way, I'm happy to see that this awful piece of code is not just my least hated part of the codebase, but also yours. :)

Long story short, unless I completely missed something, the `read_table` function doesn't have a way to know which columns are of which type. It uses `readFile()` to read the whole file, converting it to a matrix via `strsplit()` call. But my attempts at doing something like `try to convert a column to a type integer, or a float, or a logical and if an error is caught, keep it as a string` failed. Maybe I missed something?

Again, reading the data as a JSON would obviously solve this and perhaps I could even keep the current R implementation by adding one layer which translates data frame representation of the configuration to JSON.

But related to the previous point -- is there any chance that a .tsv->Dictionary loading functionality could be added to Eidos? In terms of type conversion, I think even the various .tsv loading R packages simply try to read the first N elements of a column and then decide what the type should be (unless the user specifies the column types upfront).

---

### ⏳ In-progress

> slim_time(), yikes.  That's a lot of complicated code just to translate times.  Would it be possible to do a different approach to this?  You could maybe (a) write out the translated times in the first place so they don't need to be shifted at runtime, or (b) do the shift by simply adding BURNIN_LENGTH when you *use* a time value that you just got from a table?  Don't know if any of this is practical, but this sort of complex dictionary-munging is definitely an obstacle to understanding how the script works.

Hmmm, you're very right. I agree that the time shifting shouldn't be something that should happen on the backend. I like the idea of the backend being as much of a clean tidy SLiM script as possible (except perhaps for the table-based stuff). In some way, I'm already doing some forward-vs-backward/years-ago-vs-generations voodoo on the R side anyway, precisely for this reason.

---

### ⏳ In-progress

> original_time(), I'm not really sure what it does.  In what units is "current generation time", in what units is "the original units", why are they different, etc.?  The comment on this function needs to be improved.

Same as the above. I tried to help the user keep everything they do in the "time specification" of their choosing (not sure what the right way to call this is -- I mean the direction of time in their model as forward/backward, what is the generation length, etc.), both in the model configuration step (specifying splits, gene flows, spatial dynamics, etc.) and also in the output data. What this function does is, yet again, something that should happen in the R interface.

---

> Hmm.  Well, my initial reaction is that this whole idea of "modules" etc. sounds like a large increase in complexity for slendr, and I wonder whether it will really make people happy.  There are a million different ways that people might want to extend a slendr model, and I think you might drive yourself crazy trying to support all of them.  Let slendr do what it does; it does it very well.  If people want to go beyond that, maybe it would be best for them to generate the slendr SLiM script, and then start hand-modifying it to go beyond slendr's capabilities.  Otherwise you end up trying to re-implement in slendr everything that SLiM can do, and that will never end.  If, on the other hand, you say "oh, but that kind of open-ended kitchen-sink thing isn't really what I want to do, I just want to add support for very simple selection models", then maybe a more directed fix that doesn't involve "modules" etc. would be better?  But I'm not sure, that's just my gut reaction.

We already talked about this in the last meeting but it's worth repeating here: thank you for your perspective. What you said makes perfect sense and I'm glad you said it. I should focus to make slendr to what it currently does best. If the user decides they want to
