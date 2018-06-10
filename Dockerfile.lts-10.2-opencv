FROM fpco/stack-build:latest

RUN apt-get update
RUN apt-get install -y cmake libgtk2.0-dev pkg-config libavcodec-dev libavformat-dev libswscale-dev qt5-default

RUN mkdir /opencv-build
WORKDIR /opencv-build

ADD https://github.com/opencv/opencv/archive/3.4.0.zip opencv.zip
RUN unzip opencv.zip

ADD https://github.com/opencv/opencv_contrib/archive/3.4.0.zip opencv_contrib.zip
RUN unzip opencv_contrib.zip

RUN mkdir opencv-3.4.0/build
WORKDIR opencv-3.4.0/build
RUN cmake -D WITH_IPP=ON \
      -D WITH_OPENGL=ON \
      -D WITH_QT=ON \
      -D BUILD_EXAMPLES=OFF \
      -D BUILD_TESTS=OFF \
      -D BUILD_PERF_TESTS=OFF  \
      -D BUILD_opencv_java=OFF \
      -D BUILD_opencv_python=OFF \
      -D BUILD_opencv_python2=OFF \
      -D BUILD_opencv_python3=OFF \
      -D OPENCV_EXTRA_MODULES_PATH=../../opencv_contrib-3.4.0/modules ..

RUN make -j 8

RUN make install

# Based on https://github.com/LumiGuide/haskell-opencv/blob/59208d5e32d9a99217124909431bbfd5ea4ab9f7/.travis.yml#L39-L41
# But without variable substitition, because Docker doesn't seem to support it inside and ENV:

ENV PKG_CONFIG_PATH=/usr/local/lib/x86_64-linux-gnu/pkgconfig:/usr/local/lib/pkgconfig:/usr/local/share/pkgconfig:/usr/lib/x86_64-linux-gnu/pkgconfig:/usr/lib/pkgconfig:/usr/share/pkgconfig:/root/usr/lib/pkgconfig
ENV INCLUDE_PATH=$HOME/usr/include:${INCLUDE_PATH}
ENV LD_LIBRARY_PATH=$HOME/usr/lib:${LD_LIBRARY_PATH}
