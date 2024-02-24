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

- [x] add toggle mutes

    - use mute buttons

- [ ] add switch between several tracks. Igonre the cues so far
  Just switch on a new track and turn off previous one.
    - with single button

    - with shift button (shift is used to reuse the same buttons
      and also to not to swiitch tracks by accident)

- [ ] add clips (track parts). ignore bpm sync.
  Just switch imidiately to the track part in a loop

- [ ] loop and play once options

- [ ] use BPM sync (hard task)

    - with same BPM and measure

    - with changeable BPM and measure

- [ ] config validations

- [ ] crossfades?

### v2

- [ ] add FX sends

   - configs for FXs: delay, reverb, stutter

   - sends for channels

- [ ] add master chain:
 
   - EQ

   - Compressor

   - Limiter

   - Reverb

   - Delay

- [ ] add channel tools:
    - EQ

    - Compressor

### v3

channel extras:

    - play reverse

    - play stutter

    - PAD smooth transitions

