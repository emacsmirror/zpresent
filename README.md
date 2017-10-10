# zpresent.el #

zpresent.el is a package to present from within Emacs. The presentations are based on org files.

The presentations look like this:

![zpresent sample image](http://imgur.com/stENNeV.jpg)

## Installing

### Melpa

This can be installed through package.el from [melpa](https://melpa.org/#/zpresent), or via [use-package](https://github.com/jwiegley/use-package):

    (use-package zpresent :ensure t)

## Presenting with zpresent

Open the file you want to present, and within it, run `M-x zpresent`.

## Tutorial

There's an included tutorial, which also serves as a demo. Open the file `tutorial.org`, and run `M-x zpresent` to start the tutorial.

## Input File

### Basics

The zpresent input file is an org-mode file. Each top-level headline (that is, a line beginning with a single asterisk) is the start of a new slide. The title for that slide is the headline. Each nested item under that headline is a new row in the slide. Headlines with a tag `:slide:` will be the last item in a slide, before the next headlines continue that slide. The last headline under a top-level headline is always treated as though it has a `:slide:` tag; it's optional.

To present an org file, open that file, then run `#'zpresent`.

### Example 1

This org file:

    * zpresent.el -- a new way to present!
    * Start with a title
    And have body text.

results in two slides:

1\. Title-only slide. Note that titles do not have bullets at the beginning.


```
    zpresent.el -- a new way to present!
```

2\. Second headline, title and text combined.

```
    Start with a title
    And have body text.
```

### Example 2

On the other hand, this org file:

    * zpresent.el -- a new way to present!
    ** Let's start at the very beginning
    ** A very good place to start!

results in _only a single_ slide:

1\. Title and both child bullets

```
    zpresent.el -- a new way to present!
    ▸ Let's start at the very beginning
    ▸ A very good place to start!
```

### Example 2

Finally, this org file:

    * zpresent.el -- a new way to present!    :slide:
    ** Let's start at the very beginning
    ** A very good place to start!    :slide:

results in two slides:

1\. Title-only slide


```
    zpresent.el -- a new way to present!
```

2\. Title and both child bullets

```
    zpresent.el -- a new way to present!
    ▸ Let's start at the very beginning
    ▸ A very good place to start!
```

### Links and image display

Regular org-mode links (`[[http://target-here.com][text here]]`) are inserted as links in the slide, which can be clicked on with the mouse.

The exception to this is if the text for a link is `zp-image`. If the text is only that, the target file will be opened, and inserted as an image. This works inline with text, or on a line by itself.

### Full-screen images

An image can be presented as a slide, with no text in it. Do this by creating a top-level slide, with two properties. The `type` property must be set to `full-screen-image`. The `image` property must be the address of the image. For example:

```
* This slide will display the image fullscreen, and will have no text.
:PROPERTIES:
:type:     full-screen-image
:image:    file:~/Pictures/giant-waterfall.jpg
:END:
```

### Navigating

You can move to the next slide with `n`, `C-n`, `<right>`, or `<down>`

You can move to the previous slide with `p`, `C-p`, `<left>`, or `<up>`.

### Font size

You can increase the font size with `+`, `=`, `C-+`, or `C-=`.

You can decrease the font size with `-` or `C--`.

## Version History

### 0.3

Allow for `:slide:` tags on headlines to separate slides; don't make each headline automatically a new slide.
