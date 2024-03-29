# MicroVTT

A minimalistic VTT for local play. System-agnostic, because it enforces almost no rules whatsoever. It shows battlemaps and tokens, tracks initiative and not much more. 

### How does it work?

MicroVTT comes with a controller form for the DM's laptop, and a display form for the players to watch on your second screen / TV / display table / whatever your setup looks like.

### So I cannot use this to play with others online?

I guess you could stream the player's window over the screensharing system of your choice, but remember that MicroVTT assumes that everyone can talk to each other. The only way for a player to do anything on screen is asking the DM to do it. It might work when combined with an external voice chat program, but at that point you are probably better off using a "real" VTT.

### How does it compare to other VTTs?

Favorably, I hope? But seriously, v0.1 was slapped together by one person in two weeks, I am not claiming greatness here. As said above: minimalistic.

### Which formats can I use for my maps and tokens?

As of version 0.2.1, MicroVTT supports jpg, png (including transparency), bmp, and webp (requires libwebp.dll on the system). The underlying graphics package supports more formats, such as gif, pcx, tga, tiff, and a few more, which will probably all be added sooner or later.

### Can it add grids to my maps?

Yes it can! Rectangular as well as hex grids (in both orientations) and isometric, cell size and offsets can be set independently for both directions, non-integer sizes are possible. Tokens can snap to the grid whether it is visible or not.

### Does it have any fancy map effects? Lighting, Fog of War, particle generators?

Not at this point. Lighting and FoW in particular would require walls on the maps to be marked by the DM, which is an entire thing in itself. I am working on it, it might be ready for version 0.4. For now, things the players are not meant to see can be blocked with solid black tokens as quasi-FoW.

### Does it have any tools to create my own maps or tokens, then?

Also no, it provides no content whatsoever. This is not likely to change either. Bring Your Own Battlemaps.

### So what about my tokens? Can I track my viewing direction? Add numbers to my skeleton minions? Show status effects?

Yes to all of them. Direction can be shown by rotation or a small arrow, and is not restricted to grid directions. Status effects can be shown via overlay icons (again, Bring Your Own).

### I want to show the range of my archer / the area of effect of my fireball / the field of view of a guard...

You can, as of version 0.3. Add a Range Indicator-Token onto the playing field. Hold Ctrl + drag it onto another token in order to attach it and have it move and rotate with that token.

### How about a way to show short bits of text on the map?

Also possible as of 0.3. Just drag a Text Token onto the map.

### Does it track Health, Mana or whatever else my character has?

No. Remember, we assume that everyone is at the same table. This is the player's responsibility.

### You mentioned initiative tracking earlier? How does that work, if it does not enforce any rules?

I said "almost no rules". The rules in this case are "Tokens have a base value, add a dice roll to that, highest go first". Simple. If initiative in your system goes lowest-to-highest, there is a setting for that, too. Want someone in the initiative order who is not on the map, or have tokens on the map not in the initiative list? Both are possible.

### How about that campaign planning system you mentioned?
Version 0.3 strikes again. We now have a wiki-style tool that can keep track of characters, places, objects, events or whatever else you feel like taking notes about. Articles or parts of it can be marked as master only-information. If you have a bit of webspace, you can export the entire database (or the player information only) to static html and make it available to others.

### What does the future hold? Any interesting new features in development?

Well, here are a few things that I will probably add within the next few minor versions:

- More options for the initiative list, like changing order or adding new fighters while in combat
- A dice tray (again, we expect players to bring their own, but if the table is getting crowded... not exactly a far-fetched feature.)
- Other themes, for games outside of the standard fantasy setting

And a few things that might happen, but are most likely going to take some time:

- Lighting- and Fog of War-effects. At least for uvtt-maps.
- Ambient sound generation (because I have a procedural sound-project that I would really like to revisit)
- Random generators for Names, NPCs, Taverns, whatever (There are already more than enough online, but... No, I have no justification other than "Would like to do it".)

### Anything more system-specific? Character sheet management, automatic rolls,...?

Maaaaybe. For several reasons - including legal ones - system-specific things are best done as plugins with a separate codebase. Which would require a plugin-interface first. Which would firmly be located in the second list above.

### I want to compile this for Mac / Linux / whatever, what do I need?

Other than Lazarus, the following packages:

- [BGRABitmap](https://github.com/bgrabitmap/bgrabitmap/releases)
- [HtmlViewer](https://github.com/BerndGabriel/HtmlViewer)
- [fpc-markdown](https://github.com/mriscoc/fpc-markdown)


