
# Bilder einfügen via upload-link

So sieht der HTML-code eines Bildes ein, das über die Mediathek geladen habt:


<img class="alignnone size-full wp-image-16090" src="http://pwiweb.uzh.ch/wordpress/wp-content/uploads/2018/05/insta.jpg" alt="" width="800" height="400" />

```
<img class="alignnone size-full wp-image-16090" src="http://pwiweb.uzh.ch/wordpress/wp-content/uploads/2018/05/insta.jpg" alt="" width="800" height="400" />

```

&nbsp;

If you want to add a caption, we just use `figure`-wrapper and add a `figcaption`-subelement:

```
<figure>
    <img class="alignnone size-full wp-image-16090" src="http://pwiweb.uzh.ch/wordpress/wp-content/uploads/2018/05/insta.jpg" alt="" width="800" height="400" />
    <figcaption>Caption goes here</figcaption>
</figure>
```

<figure>
    <img class="alignnone size-full wp-image-16090" src="http://pwiweb.uzh.ch/wordpress/wp-content/uploads/2018/05/insta.jpg" alt="" width="800" height="400" />
    <figcaption>Caption goes here</figcaption>
</figure>

&nbsp;

Of course, you can adjust the properties of the caption individually. Often, a caption is slightly smaller than the text, grey-ish, and centered below the image. In order to achieve this we can add the following style properties to the `figcaption`-subelement:

```
style="font-size: 12px; color: grey; text-align: center"

```

So that it looks like this:

```
<figure>
    <img class="alignnone size-full wp-image-16090"
     src="http://pwiweb.uzh.ch/wordpress/wp-content/uploads/2018/05/insta.jpg" alt="" width="800" height="400" />
    <figcaption style="font-size: 12px; color: grey; text-align: center">Caption goes here</figcaption>
</figure>
```

<figure>
    <img class="alignnone size-full wp-image-16090"
     src="http://pwiweb.uzh.ch/wordpress/wp-content/uploads/2018/05/insta.jpg" alt="" width="800" height="400" />
    <figcaption style="font-size: 12px; color: grey; text-align: center">Caption goes here</figcaption>
</figure>

&nbsp;
