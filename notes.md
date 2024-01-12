# BUGS
 * elastic and bounce :- something is off with the arithmetic :(


## player health indicator
vertical colored bar,
 100% - 75%   green, solid
 75%  - 50%   amber, alpha fading
 25%  <       red, flashing

# LEVEL UFO

become part of an attack wave, start them rotating again... the keen eyed
player will then know that they are about to turn into attackers... or maybe
slow the rotation / speed it up to normal ?!?!?!

  - attacking fire
  - power ups
  - rocket reverse thrusters -- attention to details!
  - extra guns that slide out under the wings (key 2 for testing, retract toggle)
  - amend firing so that when wing guns are on, three missiles per fire press
  - amend firing so that LIMIT takes into account wing guns on or off on check


Script:

    LIVES INDICATOR: small ship sprite and a number e.g. 5
    SHIELDS INDICATOR: small shield and a number eg. 5
    SHIP SCROLLS UP: tween up
    TEXT: "GET READY!"
        |

        make the UFO-s appear on the screen in their initial
        positions etc. colours, alphas tweens

            TEXTURE ROTATION !

        |
    When and ONLY WHEN the game state is ready can the player
    take control of the ship.

**will also need **

    OSD: for the currently playing music track, on each change
    for the OSD I can add an animtext 'marquee_lr' for an old
    school left to right sine wavey scrolling of the text, each
    letter follows start to finish on X, Y is +/- sin of x etc.


the OSD can then be put back into the splash screen so that people can know
where I got the music from and who wrote each track.


# TOPMIND
 * beziers ?!?!?
 * callbacks from tweens on 'done'? This would mean allowing the
   code to reset the tween etc or change it for another one etc.
  - tween_events: starting, ended, moving.
 * start passing gamestate to the other modules to reduce param noise

then

    SPRITE TWEENS: as for text BUT... for moving shapes on 'auto pilot', this
    is the start of flight path animation.

 * move the player to the centre, offset from screen bottom by its height plus
 some padding.

 * implement left and right motion using the keyboard, simple at first, then
 add acceleration using tweens as velocity modifiers.

 * possibly add elastic/in/out easing as well


# MP3 music was ripped as mp3 from this YT video

https://www.youtube.com/watch?v=RreryzfxxxE

- Kevin MacLeod (incompetech.com) 
  Licensed under Creative Commons: By Attribution 3.0

Full credits in-game will be given as this is great music! Tracklist (the
names of the artists are in the video, at the beginning of each song):

     Sec Mn:Sc
         00:00 Eight Bit Sahara
         03:51 Joy Riding on the Superhighway
         07:56 Itty Bitty 8 Bit
         11:09 Reformat
         14:50 Theme for Harold Var.1
         16:41 Rhinoceros
         20:05 Wagon Wheel - Electronic
         25:09 Hero's Day Off
         28:32 Wwwwub
         32:00 8-bit Dungeon Level
         35:38 Video Dungeon Crawl
         39:11 Eight Bit Stabs
         40:57 Operation Catch the Bad Guy 8 bit
         44:33 Purple Town
         47:49 Mighty Eight Bit Ranger
         50:51 Eight Bit Commando
         54:00 Chiptune Does Dubstep
         58:10 Aristocratic Festivities 8-Bit
