# deep dream Algorithm

> Jialin Lu, luxxxlucy@gmail. Implemented in Mathematica

## Implements

The deep dream algoritms simply maximize the activation.

## Usage

You basically first find a feature extracting net. And then you can proceed on processing arbitray image (3D-tensor).

```
DeepDreamMaker[img,featureNet,iterStep, octaveNumber,octaveScale,jitter]
```

## Caution Point in inplementing

1. taking the activation value before the ReLU ramping.
2. The hierachical usage of multi-scale image processing called "Octave" is very important in terms of the
3. A whole layer or a cone channel? Experiments shows that the maximization of a whole layer is not going to get good result (if you don't want to spend time on selecting a good layer)
4. Normalize the step-size is very important since a image could easily become too "contrastive".
