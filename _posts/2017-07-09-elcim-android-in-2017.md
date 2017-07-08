---
layout: post
title: Eclim + Android in 2017
comments: true
redirect_from: "/2017/07/09/eclim_android_in_2017/"
permalink: eclim-android-in-2017
---

This post tries to help to get eclim working with Android. Google does no longer support Android development in Eclipse. If you
absolutely want to use vim then the eclim plugin gives autocompletion and source code validation for Java via an eclipse server that runs in the background.
It is essentially a giant big hack, but so far worked it worked for me. I have only tested this with the headless eclipse server.

Suppose you have a working android gradle project for which you want to use eclim. First add the directory `path/to/your/gradle/project/app/src/main` as an Android Project to eclipse. Make sure your Android-SDK path is set up correctly in Eclipse. After some classpath tweaking you should be able to get most Android modules imported.
In my case, I then had problems importing the following Android Support Libraries.

```java
import android.support.annotation.NonNull;
import android.support.v13.app.FragmentCompat;
import android.support.v4.app.ActivityCompat;
```

I tracked down the libraries in question in the Android SDK directory.
In my case, they were located at
`path/to/android/sdk/extras/android/m2repository/com/android/support/`. I added them manually to the Java Build Path in Eclipse.
For some I had to extract the JAR files from the AAR files, as described in the following StackOverflow
[answer](https://stackoverflow.com/questions/21417419/how-to-convert-aar-to-jar/21485222#21485222). This solved the problem for the
first two imports, but the last import still could not be resolved. I thought that some dependencies might still
be missing and wrote the following [script](https://gist.github.com/FiV0/ac63421b9c03cae277bd356393a70d29) to extract all
the JAR class files in the `path/to/android/sdk/extras` directory. After adding all these JAR files to Java Build Path, Eclipse finally could resolve
all imports. The script is used as follows:
```
./extract_classes -src /path/to/android/sdk/dir -des /my/dir
```
It recursivly looks at all subdirectories of the specified Android SDK folder until it either finds a JAR file which is Javadoc or Source copies it
to the destination directory. If an AAR file is found the content is extracted to a folder of the same name at that location as described in the above answer and
the JAR file copied afterwards with an appropriate name to the destination directory.


My whole impression of the endeavor was that Google does not make it exactly easy to support a development environment other than Android Studio.
