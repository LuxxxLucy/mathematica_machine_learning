(* ::Package:: *)
(*
    Deep Dream Algorithm

    by Jialin Lu https://luxxxlucy.github.io
*)

DeepDreamStep[contentImg_, featureNet_] :=
 
 Module[{dims, net, trainingdata, trainStep},
  dims = Prepend[3]@Reverse@ImageDimensions[contentImg];
  net = NetGraph[
    <|"Image" ->
      ConstantArrayLayer[
       "Array" ->
        NetEncoder[{"Image", ImageDimensions[contentImg]}]@ contentImg],
     "imageFeat" -> NetReplacePart[featureNet, "Input" -> dims],
     "l2Loss" -> l2Loss
     |>,
    {
     "Image" -> "imageFeat",
     {"imageFeat", NetPort["ZeroBaseTensor"]} -> "l2Loss" }
    ];
  trainingdata = <|
    "ZeroBaseTensor" ->
    { NetReplacePart[featureNet, "Input" -> dims][ NetEncoder[{"Image", ImageDimensions[contentImg]}] @ contentImg]*0}|>;
  trainStep = NetTrain[net,
    trainingdata,
    LossFunction -> {"Objective" -> Scaled[-1]},
    LearningRateMultipliers -> {"Image" -> 1, _ -> None},
    (* TrainingProgressReporting ->
     Function[decoder[#Weights[{"Image", "Array"}]]], *)
    TrainingProgressReporting\[Rule] None,
    MaxTrainingRounds -> 300,
    Method -> {"SGD", "LearningRate" -> 25},
    TargetDevice -> "CPU"];
  ShowResult[trainStep]
  ]
