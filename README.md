# zpresent.el #

zpresent.el is a package to present from within Emacs.


## Input File

### Basics

The zpresent input file is simply an org-mode file. Each top-level headline (that is, a line beginning with a single asterisk) is the start of a new slide. The title for that slide is the headline.

This file:

    * zpresent.el -- a new way to present!
    * Start with a title
    And have body text.

results in two slides:

1. Title-only slide


```
    * zpresent.el -- a new way to present!
```

2. Second headline, title and text

```
    * Start with a title
    And have body text.
```

### Links and image display

Regular org-mode links (`[[http://target-here.com][text here]]`) are inserted as links in the slide, which can be clicked on with the mouse.

The exception to this is if the text for a link is `zp-image`. If the text is only that, the target file will be opened, and inserted as an image. This works inline with text, or on a line by itself.

## Presenting with zpresent

Open the file you want to present, and within it, run `zpresent`.

### Navigating

You can move to the next slide with `n`, `C-n`, `<right>`, or `<down>`

You can move to the previous slide with `p`, `C-p`, `<left>`, or `<up>`.

### Font size

You can increase the font size with `+`, `=`, `C-+`, or `C-=`.

You can decrease the font size with `-` or `C--`.