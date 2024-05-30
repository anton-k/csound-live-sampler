# TODO

prototypes:

### v1:

- [x] implement app that can load one track in loop
  it assigns stems to channels. and plays bakc everything as in config.

    - [x] implement csound multitrack player

    - [x] test on various files, create several tracks, configs

- [x] add volume control with MIDI. Master and channels

    - [x] implement controller in csound for AKAI midi mix
        csound midi: https://flossmanual.csound.com/midi/receiving-events-by-midiin

    - [x] use it to control volumes

    - [x] make it configurable (reasignable channels)

- [x] add gains: master, channel, tracks, stems

- [x] add toggle mutes

    - [x] use mute buttons

- [x] add switch between several tracks. Igonre the cues so far
  Just switch on a new track and turn off previous one.
    - [x] with single button

    - [x] with shift button (shift is used to reuse the same buttons
      and also to not to swiitch tracks by accident)

- [x] add clips (track parts). ignore bpm sync.
  Just switch imidiately to the track part in a loop

- [x] loop and play once options

- [x] use BPM sync (hard task)

    - [x] with same BPM and measure

    - [x] with changeable BPM and measure

- [ ] config validations

- [ ] crossfades?

### v2

- [x] add FX sends

   - [x] configs for FXs: delay, reverb, bbcuts

   - [x] sends for channels

- [ ] add master chain:
 
   - EQ

   - Compressor

   - Limiter

   - [x] Reverb

   - [x] Delay

- [ ] add channel tools:
    - EQ

    - Compressor

- [] pre-bar play in loops

- [] midi channels

### v3

channel extras:

    - play reverse

    - play stutter

    - PAD smooth transitions

    - play random cuts

    - play long samples unBPM (for PADs)

    - play random snippets in style of music for airports (for PADs and ambience)

