-*- mode:flyspell; -*-

                       Expert Guide for Windows v2.7
                       =============================

                              By Dave pearson
                             <davep@davep.org>

Introduction
============

Welcome to WEG version 2. WEG is a Norton Guide database reader for Windows.
If you don't know what a Norton Guide database is you probably don't need
WEG.

WEG is released, under the GPL, with full source code. Please take the time
to read the file COPYING if you want to further enhance this software, in
fact, why not take the time to read it anyway?

Licence
=======

WEG is released under the terms of the GPL; see the file COPYING which
should accompany this file. However, because WEG makes use of Delphi's VCL
you should consider the VCL to be "system software" in regard to the
"special exception" mentioned in section 3 of the GPL.

Please also note that WEG makes use of Toolbar2000 and does so under its GPL
licence option. See <URL:http://www.jrsoftware.org/> for more details on
Toolbar2000.

Latest Release
==============

You'll always find the latest release of WEG at:

    <URL:http://www.davep.org/norton-guides/#weg>

See the file CHANGES.txt for details of what has changed in this and
previous releases.

Fonts
=====

WEG, for obvious reasons, only lets you select fixed pitch fonts for
displaying the content of a guide. By default WEG will attempt to make use
of "Fixedsys" and the guide reading code will do OEM to ANSI translation.
Personally, for the types of guides I read (US/UK English with lots of line
drawing characters), I've found that "MS Linedraw" with OEM to ANSI
translation turned off works best. I'd suggest that you try and track down a
copy of "MS Linedraw" and use that. As best as I can tell it used to ship
with some versions of Microsoft Office. A search on Google seems to turn up
copies for download.

Please don't ask me to send you a copy of the font. I will ignore such
requests.

Building WEG
============

To build WEG you'll need Delphi. I developed WEG and WEGLib with Delphi 5, I
don't know if this code will build with later versions of Delphi (I'm pretty
certain that it won't build with Kylix). You'll then need to build and
install WEGLib. You will also need to get and install Toolbar2000. You can
get it from <URL:http://www.jrsoftware.org/>. Note that Toolbar2000 is free
to use as long as you're building free software (well, "non-profit" software
anyway).

Feedback
========

Please feel free to send me any feedback, ideas, enhancement requests, bug
reports, bug fixes (even better) or anything else. I'd appreciate it if you
sent email concerining WEG to weg@davep.org, if you just want a chat then
use the email address at the start of this file.

Before you do drop me a line please make sure that you've had a read of
TODO.txt and BUGS.txt. It might be that I'm already aware of a bug you're
about to report or I might already plan to implement some feature you're
about to ask me for.

On the off chance that you want to show your thanks in some tangible way
feel free to visit my web site and click on my picture.

Thanks
======

Thanks go to:

o Arnold Johnson for his invaluable input (and, who, sadly, is no longer
  with us to provide such invaluable input).
o Viktor Trunov for his feedback and ideas and for testing WEG v1.x against
  Russian language guides.
o The inmates of comp.lang.clipper for their feedback, ideas and support.
o Jordon Russell for Toolbar2000.
o Douglas Woodrow for his fix to saving window positions in a way that
  doesn't look odd on XP with animated Windows.

$Id$
