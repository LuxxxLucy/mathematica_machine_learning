(* ::Package:: *)

encoder = NetChain[{FlattenLayer[],LinearLayer[60],BatchNormalizationLayer[],ElementwiseLayer["ReLU"],40},"Input"-> {1,28,28}]
decoder =NetChain[
{ElementwiseLayer["ReLU"],60,BatchNormalizationLayer[],ElementwiseLayer["ReLU"],784,ReshapeLayer[{1,28,28}]},"Input"->40]
inputPreprocessor=NetEncoder[{"Image",{28,28},"Grayscale"}]

autoencoder = NetGraph[
  	<|
   	"Encoder" -> NetInitialize[ encoder],
   	"Decoder" -> NetInitialize[decoder],
   	"loss" -> MeanAbsoluteLossLayer[]
   	|>,
  	{

   	"Encoder" -> NetPort["Latent Representation"],
   	"Decoder" -> NetPort["Reconstruction"],
   	NetPort["Input"] -> "Encoder" -> "Decoder" -> "loss",
   	NetPort["Input"] -> NetPort["loss", "Target"]
   	},
  	"Input" -> inputPreprocessor,
  	"Reconstruction" -> NetDecoder[enc]
  ]

trained =
    NetTrain[autoencoder,
        <|"Input" -> trainingImages|>,
        ValidationSet -> Scaled[0.1],
        MaxTrainingRounds -> 50]
