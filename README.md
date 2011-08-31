GPS 2 HTML Report
================

This is a utility written in Haskell, to generate HTML reports from GPS track files.

Included in the report:

* Details of the journey... journey time, distance travelled etc..
* Diagrams charting speed, elevation, accumulated distance etc..
* OpenStreetMap diagram highlighting the GPS track

An example can be seen [HERE](http://www.macs.hw.ac.uk/~rs46/gps2htmlreport/3/index.html).

The Haddock documentation pages can be found [here](http://www.macs.hw.ac.uk/~rs46/gps2htmlreport/doc/).

Installation
------------
It is assumed that you have the Haskell Platform installed.

Just run these commands to configure and install the `gps2HtmlReport' utility, run these commands:

```
cabal configure
cabal install
```

This Haskell program also makes use of the bindings to **GraphcsMagick** and **Cairo**, and so the necessary system packages need to be installed, via a Linux package manager.

On an RPM-based package manager, run this command as root:

```
yum install GraphicsMagick cairo alex happy gtk2hs-buildtools
```

Prerequisites
-------------
First of all, you need to have your GPS date in a GPX file. There are many gpx exporters available. I use my Android phone to take GPX tracks, with a great application, [OSMTracker](https://code.google.com/p/osmtracker-android/). This application allows you to export your GPS tracks to GPX.

Usage
-----
The program will search for all files ending in ".gpx", and for each one, generate a HTML report.

```
$ cd $location_of_gpx_files
$ ls
1.gpx
$ gps2HtmlReport
Processing 1 file(s)...
Generating statistical charts...
Downloading OpenStreetMap tiles...
Processing '1.gpx' complete. Report saved in: /home/foo/bar/1/index.html
```

Notes
-----
This project requires testing!

If you are able to use the utility to generate HTML reports, then I'd like to hear suggestions for improvements. If you are **unable** to run it, then I **really** want to hear from you. What the problem is; How far did you get; or better still, send me the .gpx file.

I'd also like to know what is required to make this utility work on non-Linux systems. This has been tested on a Fedora Linux machine. Does it work on Mac OSX? Windows? What needs doing to run it on other Linux distro's?

Either way, get in touch!

To Do
-----

* This Haskell program currently makes use of elevation, latitude and longitude. There are many other attributes possibly available in WptType. Ideas for what to do with these attributes [here](http://hackage.haskell.org/packages/archive/GPX/0.4.8/doc/html/Data-Geo-GPX-WptType.html#t:WptType) most welcome.
* Properly attribute copyright of the OSM images to the OSM project as per their [copyright statement](http://wiki.openstreetmap.org/wiki/Legal_FAQ)

Problems
-----

If you receive this error when trying to run the program:
```
can't load .so/.DLL for: stdc++ (libstdc++.so: cannot open shared object file: No such file or directory)
```

... then you are experiencing this bug: [#5289](http://hackage.haskell.org/trac/ghc/ticket/5289).

To fix this

* Fedora 32bit:  $# ln -vs $(gcc --print-file-name=libstdc++.so) /usr/lib/
* Fedora 64bit:  $# ln -vs $(gcc --print-file-name=libstdc++.so) /usr/lib64/
* Ubuntu 32bit:  $# ln -vs $(gcc --print-file-name=libstdc++.so) /usr/local/lib/
* Ubuntu 64bit:  $# ln -vs $(gcc --print-file-name=libstdc++.so) /usr/local/lib64/


Credits
-------

Thanks to [Thomas DuBuisson](http://www.haskellers.com/user/TomMD), for implementing the `gps' package and contributing it to Hackage.