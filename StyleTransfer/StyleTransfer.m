(* ::Package:: *)
(*
    Implement Neural Style Transfer Algorithm
    Gatys et al. "A Neural Algorithm of Artistic Style"
    much of the code is based on the official tutorial
    see https://reference.wolfram.com/language/tutorial/NeuralNetworksComputerVision.html

    Organized and rewrite by Jialin Lu https://luxxxlucy.github.io
*)


(*  the definition of loss *)
contentLoss =
    NetGraph[
        {MeanSquaredLossLayer[]},
        {1 -> NetPort["LossContent"]}
    ];

gramMatrix =
    NetGraph[
        {FlattenLayer[-1], TransposeLayer[1 -> 2],DotLayer[]},
        {
            1 -> 3,
            1 -> 2 -> 3
        }
    ];

styleLoss =
    NetGraph[
        {gramMatrix, gramMatrix, MeanSquaredLossLayer[]},
        {
            NetPort["Input"] -> 1,
            NetPort["Target"] -> 2,
            {1, 2} -> 3,
            3 -> NetPort["LossStyle"]
        }
    ];

l2Loss =
    NetGraph[
        {ThreadingLayer[(#1 - #2)^2 &],SummationLayer[]},
        {
            {NetPort["Input"],
            NetPort["Target"]} ->1 -> 2
        }
    ];

tvLoss =
    NetGraph[
        <|
            "dx1" -> PaddingLayer[{{0, 0}, {1, 0}, {0, 0}}, "Padding" -> "Fixed"],
            "dx2" -> PaddingLayer[{{0, 0}, {0, 1}, {0, 0}}, "Padding" -> "Fixed"],
            "dy1" -> PaddingLayer[{{0, 0}, {0, 0}, {1, 0}}, "Padding" -> "Fixed"],
            "dy2" -> PaddingLayer[{{0, 0}, {0, 0}, {0, 1}}, "Padding" -> "Fixed"],
            "lossx" -> l2Loss,
            "lossy" -> l2Loss,
            "tot" -> TotalLayer[]
        |>,
        {
            {"dx1", "dx2"} ->"lossx",
            {"dy1", "dy2"} -> "lossy",
            {"lossx", "lossy"} ->"tot" -> NetPort["LossTV"]
        }
    ];

(* create the computation graph *)

createTransferNet[net_, content_, styleFeatDimension_] :=
 Module[{dims = Prepend[3]@Reverse@ImageDimensions[content]},
  NetGraph[<|
    "Image" ->
     ConstantArrayLayer[
      "Array" ->
       NetEncoder[{"Image", ImageDimensions[content]}] @ content],
    "imageFeat" -> NetReplacePart[net, "Input" -> dims],
    "content" -> contentLoss, "style" -> styleLoss,
    "tv" -> tvLoss|>, {"Image" ->
     "imageFeat", {"imageFeat", NetPort["ContentFeature"]} ->
     "content", {"imageFeat", NetPort["StyleFeature"]} -> "style",
    "Image" -> "tv"}, "StyleFeature" -> styleFeatDimension]]

decoder = NetDecoder[{"Image"}];

(* style transfer as an optimizaiton problem *)

StyleTranfer[contentImg_,styleImg_,featureNet_,lossSpec_] :=
    Module[{trainingdata,net,perPixel,trainedNet},
        extractFeatures[img_] :=
            NetReplacePart[
                featureNet,
                "Input" -> NetEncoder[{"Image", ImageDimensions[img]}]
                ]
                [img];
        trainingdata =
            <|
                "ContentFeature" -> {extractFeatures[contentImg]},
                "StyleFeature" -> {extractFeatures[styleImg]}
            |>;
        net = createTransferNet[featureNet, contentImg, Dimensions@First@trainingdata["StyleFeature"]];
        perPixel = 1/(3*Apply[Times, ImageDimensions[contentImg]]);
        trainedNet =
            NetTrain[
                        net,
                        trainingdata,
                        LossFunction -> lossSpec,
                        LearningRateMultipliers -> {"Image" -> 1, _ -> None},
                        TrainingProgressReporting -> Function[decoder[#Weights[{"Image", "Array"}]]],
                        MaxTrainingRounds -> 10, BatchSize -> 1,
                        Method -> {"ADAM", "InitialLearningRate" -> 0.05},
                        TargetDevice -> "CPU"
                    ];
        trainedNet
    ]

ShowResult[trainedStyleTransfer_]:=
    decoder[NetExtract[trainedStyleTransfer, {"Image", "Array"}]]
