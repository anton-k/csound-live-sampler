# csound-live-sampler

Sampler that can launch audio files and play them synchronised on BPM.
We can launch various audio clips on beat, loop them and control 
volumes of various parts of the track. Also we can apply effects
like reverb, delay, equaliser and so on. The performance is controlled
over MIDI.

Typical scenario of usage is when you have stems from the mixing stage
and you would liike to play live and be able to control volumes of various
components of the track (say turn off drums or solo parts). 
It can switch parts of the track syncronized on beat and play parts in loops.

The code is written in Haskell with the library csound-expression.
The program expects a config file which defines the scene and produces
low-level Csound code. The Csound code can be run with Csound.
Also we can generate code and play it right away.

The features:

* Mixer with individual channels for various parts of the Scene
    * in the mixer we can group channels to buses
    * apply effects to channels

* Sampler that can play parts of the tracks in loops
  synchornised on bpm, also it can launch audio clips rescaled
  to the main bpm of the currently played track

* Playlist which organizes tracks in sequence for easy switching
   with midi controller

* Useful set of effects which can be applied to channels: reverb, delays, limiter, equalizer


