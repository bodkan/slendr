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

### Done ✅

> [...] It'd be nice if your script was syntax-compliant, so maybe you can look for a different way of doing this templating?  One possibility would be to just use a placeholder that already looks like a symbol, like X_X_SEED instead of {{seed}}, or something. [...]

I have now changed the backend from an Eidos/SLiM-noncompliant code (which used those `{{var}}` R templating substitution markers which I unfortunately had no influence over) to normal SLiM command-line parameters. I had originaly a specific idea for the R templating (it was supposed to power those custom user-defined SLiM "modules" I mentioned and make them easy to work with in R) but it's an overkill in this case.

The benefit of the current approach is that once a slendr user `compile()`s a model getting a SLiM script back, then can run it from the command-line as any other SLiM script. I.e., the `slim()` runner function passes the parameters via CLI instead of substituting the template variables.

---

### Done ✅

> You seem to be using two-space indents, I'd suggest tabs?

I hope this won't ruin our friendship, but I'm a card-carrying member of the spaces-but-not-tabs camp. 😬 However, I do admit that the two-space indents come from an R style guide which doesn't fit SLiM code (or non-R code in general). I changed the code to four-space indents. Hopefully a reasonable compromise.

---

### ✅ Done

> "g1.mutationTypes[mut_types]" seems odd, shouldn't that just be "g1.mutationTypes"?  And is there a need to set their stacking policy stuff?  They will never stack anyway, right?  How could they, if new mutations never occur?

You're right. I think this might be a result of some of my very early misunderstanding of how mutation types should be handled here (going back all the way to before *slendr* was even an R package).

---

### Done ✅

> You do "N = event.getValue("N");", so N is a variable. [...] how about "pop_size" or some such?

---

### Done ✅

> add_markers() checks if (TRACK_ANCESTRY) and does nothing if it's false.  Seems weird to me; more normal would be to put the if (TRACK_ANCESTRY) outside the function, in the caller not the function.  This is because the function name suggests that the function simply "adds markers"; if it does what you have it doing, it should be named "add_markers_if_tracking_ancestry" or some such.  A nit, but this sort of thing can cause confusion.  Functions/methods should do what their name says they do, to the extent possible.  Sorry for being pedantic, I feel a little guilty about this comment.  :->

No reason to feel guilty. As a fellow nitpicker and code pedant, I appreciate this comment. 👍 In my quasi-semi-defense, I have no idea why this is even there. I think it must be a left-over of a pre-table/Dictionary iteration of the backend which probably did something slightly more in this function.

---

### In-progress ⏳

> set_coordinates() could be made vastly more efficient by vectorizing it, but if it's not a bottleneck don't worry about it, because the way to vectorize it is a little bit gross (see https://github.com/MesserLab/SLiM-Extras/blob/master/models/Recipe_15.10_OnLand.slim)

Ah, this is clever. Also, yet another case where I missed a clear vectorization possibility. I actually think it makes sense to optimize the code as much as possible. I will do some profiling (I've never actually used the SLiM profiler before 😬) and update the PR accordingly based on what I find.

---

### In-progress ⏳

> I wonder whether any of the output you do should be done with LogFile?  Or if you might want to add the option for additional output of runtime stats/metrics using LogFile?

Absolutely. That's something Peter also suggested at some point but I have never managed to get to this. Now that we are close to the final feature set, this is definitely a quality of life improvement worth revisiting.

---

### ⏳ In-progress

> slim_time(), yikes.  That's a lot of complicated code just to translate times.  Would it be possible to do a different approach to this?  You could maybe (a) write out the translated times in the first place so they don't need to be shifted at runtime, or (b) do the shift by simply adding BURNIN_LENGTH when you *use* a time value that you just got from a table?  Don't know if any of this is practical, but this sort of complex dictionary-munging is definitely an obstacle to understanding how the script works.

Hmmm, you're quite right. I agree that the time shifting shouldn't be something that should happen on the backend. I like the idea of the backend being as much of a clean tidy SLiM script as possible (well, as much as its table-driven nature allows). In some way, I'm already doing some of the forward-vs-backward/years-ago-vs-generations voodoo on the R side anyway, precisely for this reason.

---

### ⏳ In-progress

> original_time(), I'm not really sure what it does.  In what units is "current generation time", in what units is "the original units", why are they different, etc.?  The comment on this function needs to be improved.

Basically, very similar thing as the above (except converting times into the slendr model units). What this function does is, yet again, something that should probably happen in the R interface. I will probably tackle this together with the previous point.

---

### Zoom call ☎️

> Note that "asInteger({{seq_length}})" is unnecessary unless the string you put in place of {{seq_length}} is not an integer (which seems like it would be a bug?).  In Eidos, unlike in R, 10 is an integer, not numeric, and in fact 5e5 is also an integer.  Ah, but 5.5e5 is a float, which is arguably a bug in Eidos; that should be an integer too.  So OK, maybe you need that call.  Just thinking out loud, and now I'm curious whether you guys agree that 5.5e5 being float is a bug in Eidos.  :->

My vote would be for 5e5 being an integer and not a float, if only because the "numeric but not integer" thing in R bothers me a bit.

---

### Zoom call ☎️

> I see that you do "initializeMutationRate(0.0);".  So slendr simulations never have new mutations at all?  It looks like you define genomic element types only for ancestry inference, when tree sequence recording is turned off I suppose?  And I guess neutral mutations can be overlaid, with treeseq at least.

Calling `initializeMutationRate(0.0)` is a remnant of me originally not knowing what is it all that *slendr* would be doing. The original plan (which included those user-defined SLiM "modules") was that this would be flexible, including  user-defined non-neutral mutation types. Now that we agreed that the optional selection (and other) modules will not be emphasised in the first version, perhaps I could add `mutation_rate` as an optional argument of the `slim()` functions.

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

### Done ✅ & Zoom call ☎️

> evaluate_interactions() does "if (SPATIAL) sim.interactionTypes.evaluate();".  The whole existence of this function seems unnecessary to me.  If SPATIAL is F, there will be no defined interaction types, right?  So then calling "sim.interactionTypes.evaluate();" would do nothing, because sim.interactionTypes would evaluate to a zero-length vector.  Am I missing something?

Can't honestly remember why I wrote a dedicated function to this. I *think* I wasn't aware that calling a method on a zero-length vector wouldn't crash (as it would in some situations in R, for instance). There might be cases where I'm doing strange things with interactions and/or their evaluations. Might be worth keeping this in mind during the next Zoom session.

---

### Zoom call ☎️

> calc_ancestry(), interesting.  I really wish we could do this with tree-sequence recording.

Me too! I noticed you discussed this with Peter in our email thread recently. Not sure if I can help with implementing this on the SLiM/tskit level, but this would be an *amazing* feature to have. Lots of human history reconstruction deals with estimating ancestry proportions coming from a set of populations into some target group of individuals and having this built in would be a huge help. I imagine it would speed things up quite a bit too.

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
