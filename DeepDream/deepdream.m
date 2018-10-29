(* ::Package:: *)
(*
    Deep Dream Algorithm. (2015 Google)

    Implements:
        1. Deep Dream maxing l2-norm. gradient updating step.
        2. Octave (multiplie scaling)

    TODO:
        1. iterstep control. also leave out the learning rate.(decay?)
        2. with or without guide ( the idea is in Style transfer)
        3. Test with different filters

    by Jialin Lu https://luxxxlucy.github.io
*)

decoder = NetDecoder[{"Image"}];

DeepDreamStep[contentImg_, featureNet_, jitter_, iterStep_] :=

 Module[{step, absDiff, targetDimension, processingImg = contentImg,
   jitterTmp, jitterImg},
  Do[
   jitterTmp = RandomInteger[{-jitter, jitter + 1}, 2];
   jitterImg =
    ImageTransformation[ processingImg, # + jitterTmp &,
     DataRange -> Full];
   step =
    featureNet[<| "Input" -> jitterImg|>, NetPortGradient["Input"]];
   absDiff = Nest[Mean, Abs@ step, 3];
   step = step/absDiff*(1.5/255);
   step = Image[TensorTranspose[step, {3, 2, 1}]];
   targetDimension =
    Reverse@Take[Dimensions@ImageData@contentImg, 2];

   step = ImageResize[step, targetDimension, Resampling -> "Linear"];
   step =
    ImageTransformation[ step,  # - jitterTmp &, DataRange -> Full];
   processingImg = ImageAdd[processingImg, step]
   , {iterStep}

   ];
  ImageSubtract[processingImg, contentImg]

  ]

Octave[contentImg_, featureNet_, iterStep_, octave_, octaveScale_, jitter_] :=
   Module[
    {jitterTmp, diffImg, changedImg},
    If[octave <= 1,
     {
      diffImg =
       DeepDreamStep[contentImg, featureNet, jitter, iterStep];
      diffImg
      },
     {
      diffImg =
       Octave[ImageResize[contentImg, Scaled[1/octaveScale]],
        featureNet, iterStep, octave - 1, octaveScale, jitter];
      diffImg = ImageResize[diffImg, Scaled[octaveScale]];
      currentDiff =
       DeepDreamStep[contentImg, featureNet, jitter, iterStep];

      diffImg = ImageAdd[diffImg, currentDiff]
      }
     ];
    diffImg
    ]

OctaveStep[dreamSeed_, featureNet_, iterStep_, octave_, octaveScale_, jitter_] :=
 ImageAdd[
  Octave[dreamSeed, featureNet, iterStep, octave, octaveScale,
   jitter], dreamSeed]

DeepDreamMaker[dreamSeed_, featureNet_, iterStep_, octave_, octaveScale_, jitter_] :=
 Module[{result},
  result = dreamSeed;
  Do[result =
    OctaveStep[result, featureNet, iterStep, octave, octaveScale,
     jitter]
   , {1}
   ];
  result
  ]
