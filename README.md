# csound-live-sampler

Sampler that can launch audio files and play them synchronised on BPM.
We can launch various audio clips on beat, loop them and control 
volumes of various parts of the track. Also we can apply effects
like reverb, delay, equaliser and so on. The performance is controlled
over MIDI.

Typical scenario of usage is when you have stems from the mixing stage
and you would like to play live and be able to control volumes of various
components of the track (say turn off drums or solo parts). 
It can switch parts of the track synchronized on beat and play parts in loops.

The code is written in Haskell with the library [csound-expression](https://github.com/spell-music/csound-expression).
The program expects a config file which defines the scene and produces
low-level Csound code. The Csound code can be run with Csound.
Also we can generate code and play it right away.

The features:

* Mixer with individual channels for various parts of the Scene
    * in the mixer we can group channels to buses
    * apply effects to channels and buses

* Sampler that can play parts of the tracks in loops
  synchronised on bpm, also it can launch audio clips rescaled
  to the main bpm of the currently played track

* Playlist which organizes tracks in sequence for easy switching
   with midi controller

* Useful set of effects which can be applied to channels
   and controlled in real-time: reverb, delays, limiter, equalizer

* Very efficient and low on CPU and memory usage

## Installation 

To install and use this app we need to install two things:

* [stack](https://docs.haskellstack.org/en/stable/#how-to-install-stack) - haskell build tool
* [csound](https://csound.com/) - audio programming language that we use to play performance live

Often those tools are available in the package manager of your system.
After installation of the dependencies we can clone the repo
navigate to it and build from sources:

```
> git clone https://github.com/anton-k/csound-live-sampler.git
> cd csound-live-sampler
> stack build
> stack install
```

If you have not used haskell before for the first time it can take
quite some time to install because it will download and install haskell compiler
and all the dependencies. But on the second run the build is going to be very fast.

## Usage

The app expects yaml config file as input. Config file describes
the audio scene, mixer and sampler setup which files to use etc.

We can run in two modes:

* render csound file and run performance:

    ```
    csound-live-sampler run --config my-config.yaml 
    ```

* just render to csound file

    ```
    csound-live-sampler write-csd --config my-config.yaml --output live.csd
    ```

The latter option is useful if we want to run the performance on another 
computer without the need to re render it. If the config file does not change
we can just run generated file with csound:

```
csound live.csd
```

In the following sections we will look at how to describe live performances
with config file. We can also find some examples at the examples directory of this repo.

## Live performance

Top-level contains configuration of the main parts of the app:

```yaml
mixer: "..."
sampler: "..."
controllers: "..."
```

* mixer - is an audio mixer which has channels that control balance
   of some parts of the song (we can put on separate channels: drums, pads, voices, guitars and so on).
   We can control volumes of the parts in real time and the result is summed
   up to master channel which sends audio to speakers.

* sampler - describes the tracks of the performance and what bpm each track has
   and which sections we can loop and play in sequence.

* controllers section describes mappings from MIDI-controller of our choice
  to parameters of performance

### Quick start

Let's consider that we have two mixed tracks. And we have individual stems (parts of the track played by same instruments)
the stems are: drums, bass, guitars, vocals.

The first track "Pony on the moon" has 3 parts: intro, chorus, versus.
The second track "Last dance" has 4 parts: chorus-A, versus, chorus-B, versus.
The first track is 4/4 with 105 bpm and the second track is 3/4 with 135 bpm.

After mixing we have two directories with tracks:

```
tracks/
    pony-on-the-moon/
        * drums.wav
        * bass.wav
        * guitars.wav
        * vocals.wav
    last-dance/
        * drums.wav
        * bass.wav
        * guitars.wav
        * vocals.wav
```

Let's create audio performance to play those tracks live.

### Mixer section

First we will describe the mixer. We would like to be able
to control the volumes of the individual parts and be able to silence
or make some parts louder in climax points.
We will have 4 channels dedicated to each instrument group:

```yaml
mixer:
  master:
    volume: 1
  channels:
    - name: drums
      volume: 1

    - name: bass
      volume: 1

    - name: guitars
      volume: 1

    - name: vocals
      volume: 1
```

We will route instrument groups to individual channels.
If we give a name to a channel we can refer to it by name istead 
of integer identifier anywhere in the config file.

### Sampler section

Let's define how to play the tracks and loop over sections. 
For the task at hand we use sampler. Sampler can play audio files
and change sections synchronized on BPM. It means that when we request the 
next section the switch will happen not right away but when the section
will end up. This is useful to keep music on tempo.

Let's look at the config for the two tracks:

```yaml
sampler:
  dir: "../tracks"
  tracks :
    - name: "Pony on the moon"
      dir: "pony-on-the-moon"
      stems:
        - file: "drums.wav"
          channel: 1
        - file: "bass.wav"
          channel: 2
        - file: "guitars.wav"
          channel: 3
        - file: "vocals.wav"
          channel: 4
      slots:
        - bpm: 105
          measure: [4, 4]
          changeRate: 4
          cues:
            - dur: 16
            - dur: 32
            - dur: 32

    - name: "Last dance"
      dir: "last-dance"
      stems:
        - file: "drums.wav"
          channel: 1
        - file: "bass.wav"
          channel: 2
        - file: "guitars.wav"
          channel: 3
        - file: "vocals.wav"
          channel: 4
      slots:
        - bpm: 135
          measure: [3, 4]
          changeRate: 12
          cues:
            - dur: 24
            - dur: 12
            - dur: 24
            - dur: 12
```

The sampler defines a sequence of **tracks** also we define
a directory where all tracks are placed (this parameter is optional).
Each track has it's own directory (`dir`), a name a list of stems 
(files which contain individual instrument parts of the track)
and list of slots with cues. Slots define the division of the track on sections.

Regarding stems both tracks have the same structure. We have 4 instruments
at play: drums, bass, guitars and vocals. Each instrument is directed
to a dedicated channel:

```yaml
      stems:
        - file: "drums.wav"
          channel: 1
        - file: "bass.wav"
          channel: 2
        - file: "guitars.wav"
          channel: 3
        - file: "vocals.wav"
          channel: 4
```

Also we can write it with channel names:

```yaml
      stems:
        - file: "drums.wav"
          channel: drums
        - file: "bass.wav"
          channel: bass
        - file: "guitars.wav"
          channel: guitars
        - file: "vocals.wav"
          channel: vocals
```

The sections of the songs are different. The first track has 3 parts:

```yaml
      slots:
        - bpm: 105
          measure: [4, 4]
          changeRate: 4
          cues:
            - dur: 16
            - dur: 32
            - dur: 32
```

The cues contain the sizes in beats for the sections.
The `changeRate` defines in beats at which rate we would like
to change the parts. So when we press the button to go to the next
section it will switch only when we are on certain beat defined by the change rate.

The second track has 4 parts:

```yaml
      slots:
        - bpm: 135
          measure: [3, 4]
          changeRate: 12
          cues:
            - dur: 24
            - dur: 12
            - dur: 24
            - dur: 12
```

Why do we need slots and cues? The slots are useful when measure or
bpm changes between the parts. What if the first two sections are in 3/4 and 130 bpm
and the second two sections are in 4/4 and 100 bpm. We can describe it 
with this config:

```yaml
      slots:
        - bpm: 135
          measure: [3, 4]
          changeRate: 12
          cues:
            - dur: 24
            - dur: 12
            - dur: 16
            - dur: 8
        - bpm: 100
          measure: [4, 4]
          changeRate: 12
          cues:
            - dur: 16
            - dur: 8
```

With the sampler we can trigger songs or various parts of the track.
With this config each section is played in loop until the next one is requested.
Also we can make it automatically switch to the next section if it reaches
an end of the current section or stop. So each section can be played in loop
or reach for the next one or reach the stop at the end.

### Midi controllers

So far we have described the structure of the tracks.
Let's make it interactive and play it live with a midi controller.

Each midi controller is different so we are going to describe the mapping
for a given midi controller to parameters of the performance.

I'm going to take Akai MIDIMIX controller as an example. But we 
can adapt it to any other controller.

Let's describe first what parameters we would like to control:
* volume of the master (the final volume of the audio)
* volume of each channel
* switch tracks back and forth
* switch parts back and forth

So we would like to control the volumes and be able to 
navigate to the tracks in the playlist or some parts of the tracks.

Controllers are defined in the section: 

```yaml
mixer: ...
sampler: ...
controllers:
  midi:
    knobs: ...
    notes: ...
```

We have two main sections for the midi controllers:

* `knobs` - continuous control signals
* `notes` - trigger events (like button press, or playing notes)

To control volumes we use `knobs` and to switch between tracks we use `notes`.

#### Control the volumes

Let's define a mapping from midi-controller to volume.

```yaml
controllers:
  midi:
    knobs:
      - when:
          key: 19
        act:
          - on:
              channelVolume: 1
      - when:
          key: 23
        act:
          - on:
              channelVolume: 2
      - when:
          key: 27
        act:
          - on:
              channelVolume: 3
      - when:
          key: 31
        act:
          - on:
              channelVolume: 4
```

We have defined mappings for 4 channels of our mixer.
The knob has list of mappings from midi CC keys to parameters of the scene.

```yaml
- when:
    key: midiKeyId
    act:
      - on:
          channelVolume: channelId
```

So for example when CC 19 changes we change the volume of the channel 1.
We can control several parameters with a single controller so
the `act` object expects a list of things to control.

Let's define the volume control for the master channel:

```yaml
      - when:
          key: 62
        act:
          - on: masterVolume
```

#### Switch tracks

To switch tracks we use `notes` section of the midi controller.
The sampler has a playlist inside it of the tracks. 
With playlist we can 

* select song by it's number in sequence
* go to the next or previous section in the track 
* go to the next or previous track in the list
* select a part of the track

We have two tracks to choose. Let define how to switch between them:

```yaml
controllers:
  midi:
    notes:
      - when:
          key: 6
        act:
          - track: 1
      - when:
          key: 9
        act:
          - track: 2
```

When key 6 is pressed on a midi controller we switch to the start
of track 1, when key 9 is pressed we switch to the track 2.

#### Switch parts of the tracks

Let's go to next or previous section in the song:

```yaml
      - when:
          key: 25
        act:
          - shiftPart: -1
      - when:
          key: 26
        act:
          - shiftPart: 1
```

The parameter `shiftPart` defines on how many steps forward or backward to go in 
sections of the tracks. A minus sign makes it go backwards.
If we use `shiftTrack` we define the step across tracks.

#### Let's add a reverb to vocals and guitars

Let's look at how to use effects. We will add a reverb to the vocals and 
guitars. Let's recall the definition of the mixer:

```yaml
mixer:
  master:
    volume: 1
  channels:
    - name: drums
      volume: 1

    - name: bass
      volume: 1

    - name: guitars
      volume: 1

    - name: vocals
      volume: 1
```

Effects are added to channels. To add reverb we add a new channel
which will contain the reverb effect and send signals to it from other channels.

```yaml
mixer:
  master:
    volume: 1
  channels:
    - name: drums
      volume: 1

    - name: bass
      volume: 1

    - name: guitars
      volume: 1
      sends:
        - channel: 5
          gain: 0.2

    - name: vocals
      volume: 1
      sends:
        - channel: 5
          gain: 0.3

    - name: reverb-bus
      volume: 1
      fxs:
        - reverb:
            name: reverb
            dryWet: 1
            size: 0.5
            damp: 0.75
```

We have added a channel named `reverb-bus`. It's 5th channel. 
It has an `fxs` section which contains a sequence of audio processor.
In our case it contains the reverb. In this section we specify initial parameters of 
the reverb. 

Also we define sends which send portion of the channel output to
another channel. We send some portion of signal from guitars and vocals
channels to the reverb bus. Also we can add FXs to the master channel.
For example the typical effect to add is a limiter on master:

```yaml
master:
    volume: 1
    fxs:
      - limiter:
          name: master-limiter
          maxVolume: 0.95
```

We have studied the main features of the app and defined 
a setup for live-performance. You can find more elaborate examples
of config files at the directory [example](https://github.com/anton-k/csound-live-sampler/tree/main/example) in the repo.

To run the performance we can run the command:

```
> csound-live-sampler run --config 2-songs.yaml
```

### Mixer 

The mixer has several channels that control volumes of 
various instruments of the track. And also we have one channel
called master to which all outputs of other channels are summed up.
And master channel feeds audio to speaker.

Mixer config is:

```yaml
mixer:
  master: "..."
  channels:
    - channel-1-config
    - channel-2-config
    - channel-3-config
    - ...
```

The mixer has one master-channel and as many as you like ordinary channels.

Let's discuss the master channel config first:

```yaml
mixer:
  master: 
    volume: 1.0
    gain: 0.9
    fxs: 
      - fx-1
      - fx-2
```

#### Master channel

The master has parameters:

* **volume** - (required) initial value for the volume, range (0, 1). We can change it in real-time.
* **gain** - (optional) constant scaling factor of the audio output. If missing the 1 is the default.
* **fxs** - (optional) parameter with list of effects. We will discuss effects
   in a separate section.

#### Channels 

Let's look at the parameters of the channel:

```yaml
mixer:
  master: ...
  channels:
    - volume: 0.5
      gain: 1.0
      output: 2
      sends:
        channel: 4
        gain: 0.3
      fxs:
        - fx-1
        - fx-2
      name: drums
```

We have only one required parameter:

* **volume** - initial volume of the channel

Other arguments are all optional:

* **gain** - constant scaling factor of the volume
* **output** - if missing the output of the channel is sent to master
  otherwise we can specify channel to receive the output. 
    The output is useful to group channel output to one channel.
    For example we can have parent drum channel and children channels
    like base drum, snare, overheads all will send output to the drum channel.
* **sends** - with send we can send a portion of the signal to another channel.
   The portion is prior to scaling by volume. This is useful to create effect sends.
   For example instead of adding reverb to every instrument we
   can create a single channel dedicated to reverb processing. And we can
   send portion of the output from all instruments to that channel.
* **fxs** - a list of effects to process on channel. 
* **name** - optional name of the channel which can be added for readability.

The minimal config for mixer can look like this:

```yaml
mixer:
  master: 
    volume: 1
  channels:
    - volume: 0.5
    - volume: 0.5
    - volume: 1
    - volume: 0
```
    
We have defined a master and four channels which all are routed to master.
We will discuss how to control mixer parameters in real-time in the section
dedicated to controllers.

#### Effects

We can put a sequence of effects (audio processors) on channels.
There are several effects implemented in the library:

* tool (volume / gain / pan)
* reverb
* delay
* ping-pong delay
* moog filter
* korg ms 20 filter
* limiter
* parametric equalizer
* bbcuts (playing audio with stutter loops)

An effect has parameters which can be controlled in real-time.
Effects can be added to master channel or any channel of the mixer.

Let's add reverb to the master channel:

```yaml
mixer:
  master: 
    volume: 1
    fxs: 
        - reverb:
            name: master-reverb
            size: 0.5
            damp: 0.7
            dryWet: 0.25
  channels:
    - name: vocal
      volume: 1
    - name: guitar:
      volume: 1
```

So the typical structure of effect object is:
    
```yaml
fxs:
    - effectType:
        name: effectName
        param1: initialValue1
        param2: initialValue2
```

A value for parameter is float in range (0, 1).
Let's look at the available effects.

##### Reverb

Reverb is based on Csound opcode [reverbsc](https://csound.com/docs/manual/reverbsc.html).
It is high-quality reverb based on work of Sean Costello (the author of Valhalla plugins).

```yaml
- reverb:
    name: master-reverb
    size: 0.5
    damp: 0.7
    dryWet: 0.25
```

The reverb has 3 parameters: 

* **size** - reverb space size (from room to cave)
* **damp** - how much sound is reflected from the walls (the lower the more sound is damped)
* **drtWet** - mix between dry and wet signal (0 - no processing applied, 1 - only processed signal is out)

##### Delay

Delay is a single line delay with equalization of the reflections.
It is synchronized on bpm and `repeatTime` is measured in beats:

```yaml
delay:
    name: some-delay
    repeatTime: 0.5
    damp: 0.8
    feedback: 0.2
    dryWet: 0.0
```

* **repeatTime** - time between repeats, measured in beats (remains constant during performance)
* **damp** - how much sound is reflected. It controls of low-pass filter cutoff. 
* **feedback** - feedback of delay, how many repeats are audible. It is gain that is successfully applied to repetitions of the signal.
* **dryWet** - dry wet ratio

##### Ping-pong delay

Implements stereo ping pong delay. Reflections are audible with alternate panning.

```yaml
pingPong:
    name: some-delay
    repeatTime: 0.5
    damp: 0.8
    feedback: 0.2
    width: 0.7
    dryWet: 0.0
```

Parameters are the same as for `delay`. We have one additional parameter `width`
which defines the width of stereo spread.

##### Tool

Inspired by the Bitwig plugin `tool`. It gives basic controls for the audio:

```yaml
tool:
    name: some-tool
    volume: 1
    gain: 0.5
    pan: 0.5
    width: 0.3
```

We can change volume, gain, panning and stereo width of the signal.

##### Limiter

Useful to put on master to avoid audio clipping and distortion when 
audio exceeds the volume limit.

```yaml
limiter:
    name: some-limiter
    maxVolume: 0.95
```

##### Equalizer

Equalizer can have as many points as we would like.

```yaml
eq:
    name: some-eq
    maxGainDb :: 12 
    points:
        - mode: lowShelf
          frequency: 0.1
          gain: 0.6
          width: 0.5  
        - mode: bandPass
          frequency: 0.4
          gain: 0.5
          width: 0.5  
        - mode: highShelf
          frequency: 0.6
          gain: 0.4
          width: 0.5  
```

Parameters:

* **maxGainDb** - range of the EQ gain in decibels
* **points** - Eq points. A point has parameters:
    * **mode** - mode of the filter can be: `lowShelf`, `highShelf`, `bandPass`.
    * **frequency** - frequency of the EQ-filter
    * **gain** - gain in range `[-maxGainDb, + maxGainDb]`. It ranges in the interval `(0, 1)`.
       The middle point `0.5` means no processing is applied. Higher than 0.5 is boost and lower is attenuation.
    * **width** - width of the band of Q-factor of the filter. The `0.5` - is no Q-peak.
        
##### Resonant filters 

We have two variations of the resonant filters:

* `moogFilter` - emulator of the moog filter
* `korgFilter` - emulator of korg 35 filter (used in early versions of Korg MS 20).

They are both low-pass filters. They have distinct character.
They have the same set of parameters:

* **cutoff** - filter cutoff (range is `(0, 1)`)
* **resonance**  - filter resonance (range is `(0, 1)`)
* **dryWet** - dry wet ratio (optional, the default is 1).

Example:

```yaml
moogFilter:
    name: moog-filter
    cutoff: 0.7
    resonance: 0.15
```

##### Bbcuts

Bbcuts is a looping-stutter machine. It chops incoming signal to
chunks and plays them with repeats. It is implemented in Csound
so it's better study the original [docs](https://csound.com/docs/manual/bbcuts.html).
It is synchronized on current BPM of the track.

Example:

```yaml
bbcut:
  name: drum-bus-stutter
  subdiv: 8
  barlength: 4
  phrasebars: 1
  numrepeats: 8
  dryWet: 1
```

All parameters except `dryWet` are fixed. Other arguments control
how to slice audio to chunks and repeat it. Study the csound docs.
So we set it up and can control how much signal is fed into bbcut effect.

### Sampler

Sampler has two parts:

* An **audio engine** can play parts of audio files and synchronize on BPM

* A **playlist** of tracks which can switch between tracks and parts of the tracks

The sampler has internal metronome which syncs the launches of audio files.
For sampler we define which tracks do we have which parts each track have
and what BPM we use. 

Let's look at the sections of the sampler config:

```yaml
sampler:
  tracks: [list of tracks]
  dir: tracks-directory
```

The `dir` tells where the audio files are stored. In tracks we specify the sequence of tracks
how we can play them.


```yaml
tracks:
  - dir: track-directory
    name: name of the track
    stems: [...]
    slots: [...]
```

The track has directory `dir` where all files for that track are stored.
We assume that after mixing stage we have stem files for the track.
In stem file we have groups of alike instruments: vocals, drums, guitars.
The granularity depends on your approach to playing. Splitting to stems
gives us ability to control volumes and apply effects to certain groups of instruments
in the track. 

Slots define the division of track to parts. Like we have intro, chorus A, versus A, chorus B, versus B and outro.

#### Stems

Stems allow us to control instrument groups of the track and remix it live.
Let's look at example of the stems:

```yaml
stems:
  - file: "Arps.wav"
    channel: 4
  - file: "Kick.wav"
    channel: 1
    gain: 1.2
  - file: "Piano.wav"
    channel: 4
  - file: "Bass.wav"
    channel: 3
```

Each stem has parameters of `file`-path and mixer channel to route the audio.
Here we play Kick in channel 1, play bass on channel 3 and arps and piano on channel 4.
Also there is optional argument gain which can scale the volume of the stem file.

#### Slots

Slots define division of the track to parts in time domain.

```yaml
slots:
  - bpm: 85
    measure: [4, 4]
    changeRate: 4
    cues:
    - dur: 16
    - dur: 32
    - dur: 32
```

Our song has BPM 85 and measure 4/4. We set `changeRate` to 4. It means that 
audio clips are changed every 4th beat.
The section `cues` defines the parts of the track. In the example we have 3 parts
each 16 beats long.

Why do we need slots as a list? Because the tempo and measure of the song can change.
In this case we will use several slots.

A cue has optional arguments:

```yaml
cues:
    - start: 4
      dur: 16
      nextAction: loop
```

Start defines delay in beats from the end of previous cue/slot.
The `nextAction` has 3 modes: loop, next, stop. By default it is `loop`.
Which means we play this cue in loop until the next one is requested on playlist.
The `next` mode can be useful for intros. With it we play next part as soon as the current one ends.
The `stop` stops the playback when end is reached.

### Controllers

The section `controllers` defines how we can control parameters in real-time.

#### Midi controllers

We can control performance with MIDI-devices. We specify the parameters to control in the section:

```yaml
controllers:
    midi: 
        knobs: ...
        notes: ...
        modifiers: ...
        keys: ...
```

We have two types of midi controllers:

- knobs -  control signals
- notes - events that trigger instant change

The `modifiers` is a section for modifier keys. If we press modifier key
than we can choose different type of parameter. An example for the modifier:

With `keys`  we can give a readable name to a MIDI controll id.
So we can refer to it not by integer but by a name.

```yaml
modifiers:
  shift:
    key: 27
```

Modifiers contains object where fields are unique modifier names
and key is a Midi controller CC number.

##### Midi signals (knobs)

Let's look at the knob config. It is a list of links from midi event to parameter of the performance:

```yaml
knobs:
  - when: 
      key: 62
    act:
      - on: 
            channelVolume: 1
```

In this example when midi controller with id 62 changes we 
change the volume of the first channel. The range of the control signal is `(0, 1)`.

Let look at what we can control:

* master volume:

    ```yaml
    on: masterVolume
    ```

* channel volume:

    ```yaml
    on: 
      channelVolume: 3
    ```

* channel send

    ```yaml
    on:
      channelSend:
        from: 2
        to: 3
    ```
* effect parameter:

   ```yaml
   on:
     fxParam: 
        name: effectName
        param: paramName
   ```

* audio input gain:
    
    ```yaml
    on:
      audioInputGain: 2
    ```

All control signals by default are in range `(0, 1)`.
We can change that with optional field `range`:

```yaml
when:
  key: 25
act:
  - on: 
      channelVolume: 3
      range: [0.5, 2]
```

##### Midi events (notes)

The event defines some instant change. It looks like the knob control:

```yaml
notes:
  - when:
      key: 23
    act:
      - mute: 1
```

In this example when button with id 23 pressed we toggle mute the channel 1.
Let's look at what we can control:

* toggle mute of the channel 

    ```yaml
    mute: channelId
    ```

* set track to id:
    
    ```yaml
    track: trackId
    ```

* set track part to id (part id is int within current track):

    ```yaml
    part: partId
    ```

* go to next track relative to current one:
    
    ```yaml
    shiftTrack: intAmount
    ```

* go to next track's part relative to current one:
    
    ```yaml
    shiftPart: intAmount
    ```
  in shift negative amount goes backwards.

##### Midi key optional arguments

For midi key we can specify optional arguments:

For notes:

```yaml
when:
  key: 12
  modifier: shift
  press: off
  channel: 2
```

* `modifier` activates this trigger only if modifier is pressed at the same time.
* `press` defines note `on` or `off` mode.
* `channel` - midi-channel to listen to, by default  it listens on all channels.

For the knobs we have the same optional arguments except `press`. 

