---
layout: post
title: Universal Ace Jump
comments: true
redirect_from: "/2022/01/16/universal_ace_jump/"
permalink: universal-ace-jump
---

When working I usually have a handful of applications open. These will most likely be Emacs, a terminal, a browser,
some chat software (i.e. Slack) and maybe some other windows (Spotify if I listen to music, Wireshark if I try to debug some network issue,
Shutter I want to take a screenshot and so on...). The later ones are only opened in cases I really need them.

Let's try to figure out if there are some common organizational patterns across the former applications.
On the upper most level we have a couple of applications that we want to switch between and maybe launch some other (not yet open) application.
Gnome's default settings come with a nice integration to switch between open applications via `Super + num`.

For the browser (Firefox in my case), I mostly have quite a few tabs open, I often use
[Multi-Account-Containers](https://addons.mozilla.org/de/firefox/addon/multi-account-containers/) so I can quickly close a bunch of tabs (work related)
and have the possibility to be signed into multiple accounts at the same provider. So here are actually two levels in the organizational hierarchy,
mainly containers (grouping a bunch of tabs) and then just tabs. Here it would also be nice to switch efficiently between tab groups (containers)
and then further down between tabs.

In Emacs, I will most likely have a bunch of buffers (think of them like tabs) open. They are also grouped like in Firefox.
The main group is for files I modifying (source code, text files, etc..). A group is for REPL related buffers, the repl itself, error buffer,
a result buffer in which I might want to display some data. A magit group in which buffers will be displayed the relate to the Git workflow of
the project I am currently on. The remaining buffers go into a default group. Once again two levels of organizational structure, buffer groups and buffers.

The terminal is the entrypoint to my file system. On average, I think, I have like three terminals (terminal tabs) open, where I either launch some
script or might observe the output from some local or remote system I am using. So one level I want to switch between my terminal tabs and on a the level below I will
most likely switch between folders in my file system.

For Slack you mostly follow a bunch of channels which you want to switch between. In a channel there might be threads that you want to follow or comment on.
Let's take one more example like Spotify, you have a some artists you follow, some playlists you like. For every artist there is a bunch of albums to choose
from. Let's say you use Notion in the browser. Than the hierarchy is further expanded down. In Notion you have pages and subpages.
You want to switch between these pages. You want to search the pages.

I think the pattern that emerges is clear. At the top sit a couple of applications which you want to switch between. Even if you are not using Firefox, you are very likely
using some browser. If you are not using Emacs, you are very likely using some editor and you will also have some organizational structure for editing
, deploying, commiting, reviewing things there. If it's not Slack, you are very likely using some other chat software with a similar tree like topic structure.

What is also clear is that most likely all of the above "problems" have some solution in their domain (for example for switching between directories,
you can you fzf together with fd). What is actually the annoying part is that you need to configure all of the existing solutions yourself. In many cases
it's not even sure that the shortcuts will be available in the app you are using or if you can overwrite them.
It might be that your workflow is different you might not always group things like in the examples above, but some hierarchical structuring of applications,
tabs and subtabs is very likely also the case for you.

* Generalization paragraph
* Firefox -> browser
* Emacs -> editor
* Terminal -> might be intergrated


Introduce `ace-jump` and `ace-window`.

I don't know where the concept was created but I first got to know about it via [ace-jump](https://github.com/winterTTr/ace-jump-mode). The idea is that
the ace-jump command specify some character and your text window gets overlayed with a face, such that at all places a

![ace-jump](assets/ace_jump.gif)


* launching applications - highest level
* subwindows - new subwindow, switch subwindow based on ace-jump or ace-window
* jumping
* subcommands
