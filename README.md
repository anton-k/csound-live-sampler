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
    * apply effects to channels and busses

* Sampler that can play parts of the tracks in loops
  synchornised on bpm, also it can launch audio clips rescaled
  to the main bpm of the currently played track

* Playlist which organizes tracks in sequence for easy switching
   with midi controller

* Useful set of effects which can be applied to channels
   and controlled in real-time: reverb, delays, limiter, equalizer

* Very efficient and low on CPU and memory usage

## Installation 

Top install and use this app we need to install two things:

* [stack](https://docs.haskellstack.org/en/stable/#how-to-install-stack) - haskell build tool
* [csound](https://csound.com/) - audio programming language that we use to play performance live

Often those tools are available in the package manager of your system.
After installation of the dependencies we can clone the repo
navigate to it andbuild from sources:

```
> git clone https://github.com/anton-k/csound-live-sampler.git
> cd csound-live-sampler
> stack build
> stack install
```

If you have not used haskell before for the first time it can take
quite some time to install because it will download and install haskell compiler
and all the dependencies. But on second run the build is going to be very fast.

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
computer without the need to rerender it. If the config file does not change
we can just run generated file with csound:

```
csound live.csd
```

in the following sections we will look at how to describe live performances
with config file. We can also find some examples at the examples directory of this repo.

## Live performance

Top-level contains configuration of several main parts of the app:

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

* controllers describes mappings from MIDI-controller of our chice
  to parameters of performance

Let's discuss confir for each of this section.

### Mixer 

### Sampler

### Controllers
