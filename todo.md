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

* [ ] OSC constrols
* [ ] Looper FX synced on BPM
* [ ] metronome output
* [ ] UI sketch to control over OSC
* [ ] OSC sensors for channels volumes, master volume, maybe spectrum
* [ ] Pads playback. Play files that are not synced on bpm
* [ ] Rhytmic tremlol FX
* [ ] Cross fade and XY-pad controls
* [ ] play random short snippets of the file on event 

### v3

* [ ] play random snippets in style of music for airports (for PADs and ambience)
* [ ] UI app with OSC
* [ ] haskell lib for OSC controlling from ghci
* [ ] LFO controls
* [ ] sound fonts
* [ ] midi clips for sampler

