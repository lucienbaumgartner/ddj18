
# Bilder einfügen via upload-link

So sieht der HTML-code eines Bildes ein, das über die Mediathek geladen habt:


<img class="alignnone size-full wp-image-16090" src="http://pwiweb.uzh.ch/wordpress/wp-content/uploads/2018/05/insta.jpg" alt="" width="800" height="400" />

```
<img class="alignnone size-full wp-image-16090" src="http://pwiweb.uzh.ch/wordpress/wp-content/uploads/2018/05/insta.jpg" alt="" width="800" height="400" />

```

Falls wir Unetrtitel hinzufügen wollen, dann geht das mit folgender Ergänzung:

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


Of course, you can adjust the properties of the caption individually. Often, a caption is slightly smaller than the text, grey-ish, and centered below the image. In order to achieve this we can add the following style properties to the `img`-element:

```
style="font-size: 12px; color: grey; text-align: center"

```

So that it looks like this:

```
<figure>
    <img class="alignnone size-full wp-image-16090"
    style="font-size: 12px; color: grey; text-align: center" src="http://pwiweb.uzh.ch/wordpress/wp-content/uploads/2018/05/insta.jpg" alt="" width="800" height="400" />
    <figcaption>Caption goes here</figcaption>
</figure>
```

<figure>
    <img class="alignnone size-full wp-image-16090"
    style="font-size: 12px; color: grey; text-align: center" src="http://pwiweb.uzh.ch/wordpress/wp-content/uploads/2018/05/insta.jpg" alt="" width="800" height="400" />
    <figcaption>Caption goes here</figcaption>
</figure>
