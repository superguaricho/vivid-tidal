(
s.reboot { 
  s.options.numBuffers = 1024 * 256; 
  s.options.memSize = 8192 * 32; 
  s.options.numWireBufs = 2048; 
  s.options.maxNodes = 1024 * 32; 
  s.options.numOutputBusChannels = 2;
  s.options.numInputBusChannels = 2; 

  s.waitForBoot { 
    ~dirt.stop; 
    ~dirt = SuperDirt(2, s); 
    ~dirt.loadSoundFiles; 
    ~dirt.start(57120, 0 ! 12); 
    SuperDirt.default = ~dirt;
    s.latency = 0.3;

    // --- Vivid: Reload Listener (Robust Version) ---
    OSCFunc({ |msg|
       "Vivid: Syncing Server and SuperDirt...".postln;
       fork {
          // 1. Tell the server to load all files from disk
          s.sendMsg("/d_loadDir", "/home/numa/.local/share/SuperCollider/synthdefs/");
          s.sync;

          // 2. Tell sclang to read them into its library
          SynthDescLib.global.read; 
          0.2.wait; 

          // 3. Tell SuperDirt to update its dictionary
          ~dirt.loadSynthDefs; 
          "Vivid: Sync complete. All definitions ready.".postln;
       }
    }, '/vivid/reload').fix; 
    // ---------------------------
  }; 
};
)
