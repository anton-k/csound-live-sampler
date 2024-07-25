# TODO

prototypes:

### v1 (done):

V1 implements cli app that can run live performances with features:

* sampler - launches audio files based on BPM
* playlist - navigate track list with midi controls
* mixer - mix several audio inputs to single output and apply effects
* some effects: reverb, delay, eq, limiter, bbcuts, moog filter, korg filter
* audio card setup: read from sound card, and output to sound card
* midi controls

### v2

* [x] OSC constrols
* [x] haskell lib for OSC controlling from ghci
* [x] UI sketch to control over OSC
* [x] metronome output
* [ ] UI FXs (accordion)
* [ ] Looper FX synced on BPM
* [ ] OSC sensors for channels volumes, master volume, maybe spectrum
* [ ] Pads playback. Play files that are not synced on bpm
* [ ] Rhytmic tremolo FX
* [ ] Cross fade and XY-pad controls
* [ ] Envelops that depend on clip-transitions (for pre-tackts)
* [ ] configurable UI 
       * [ ] colors
       * [ ] layout of the elements
* [ ] Beat-repeater effect

### v3

* [ ] play random snippets in style of music for airports (for PADs and ambience)
* [ ] play random short snippets of the file on event 
* [ ] play random short files from the list

### v4

* [ ] UI app with OSC
* [ ] LFO controls
* [ ] sound fonts
* [ ] midi clips for sampler

