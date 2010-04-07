-module(vectorspace).
-export([vector_dimension/2,dot_product/2,test_dotproduct/0,relative_magnitude/2,create_idf_map/1,test_vectorspace_query/0,vectorspace_add/2,vectorspace_query/3,test_idf_map/0,create_vector/1,vector_dimensions/2,reduce_vectorspace/1,test_reduce/0,filter_vectorspace/2]).
-record(dimension,{dimensionid="00000000000000000000000000000000",value=0}).
-record(vector,{vectorid="00000000000000000000000000000000",dimensions=[]}).
  

%% @TODO Let Mike document this

create_vector(VectorID) -> #vector{vectorid=VectorID}.
vector_dimensions(V,Dims) -> V#vector{dimensions=Dims}.

%% ******************************* GENERAL PURPOSE FUNCTIONS
vector_dimension(Name,Value) ->
    #dimension{dimensionid=Name,value=Value}.

%% ******************************* DOT PRODUCT
dimension_sum_value ([TOTAL|V]) -> TOTAL#dimension.value + dimension_sum_value(V);
dimension_sum_value ([]) -> 0.
dot_product_list(VectorA,VectorB) ->
    [ {dimension,A#dimension.dimensionid,(A#dimension.value * B#dimension.value)} || A <- VectorA#vector.dimensions, B<-VectorB#vector.dimensions ,A#dimension.dimensionid==B#dimension.dimensionid ].
dot_product(VectorA,VectorB) ->
    Dims = dot_product_list(VectorA,VectorB),
    dimension_sum_value(Dims).

%% ******************************* RELATIVE MAGNITUDE
dimension_sum_sqr  ([TOTAL|V]) -> (TOTAL#dimension.value * TOTAL#dimension.value) + dimension_sum_sqr(V);
dimension_sum_sqr  ([]) -> 0.
dimensions_B_which_exist_in_A (VectorA,VectorB)->
    [ B || A <- VectorA#vector.dimensions, B<-VectorB#vector.dimensions ,A#dimension.dimensionid==B#dimension.dimensionid ].
relative_magnitude (VectorA,VectorB)->
    Dims = dimensions_B_which_exist_in_A (VectorA,VectorB),
    math:sqrt(dimension_sum_sqr(Dims)).

%% ******************************* DIMENSIONAL RELATIVITY (# of dims difference)
dimension_relativity (VectorA,VectorB) ->
    Dims = dimensions_B_which_exist_in_A (VectorA,VectorB),
    length(Dims).

%% ******************************* RELATIVE QUERY COSINE - the "score" of two vectors.
relative_query_cosine(VectorA,VectorB)->
    M1 = relative_magnitude(VectorA,VectorB),
    M2 = relative_magnitude(VectorB,VectorA),
    MT = M1 * M2,
    if (MT/=0)->
            DotProduct=dot_product(VectorA,VectorB),
            Result = DotProduct / MT,
            Arel = length(VectorA#vector.dimensions),
            if
                (Arel/=0)->
                    Brel = dimension_relativity(VectorA,VectorB),
                    Relativity = (Brel / Arel);
                (Arel==0)->
                    Relativity = 0
            end,
            (Relativity * Result);
       (MT==0)->
            0
    end.

test_dotproduct() ->
    VA = #vector{vectorid= "somevector" },
    VB = #vector{vectorid= "somevector2" },
    Dims = [vector_dimension("dog",4),vector_dimension("cat",3)],
    VAb =VA#vector{dimensions=Dims},
    Dims2 = [vector_dimension("dog",4),vector_dimension("cat",6)],
    VBb =VB#vector{dimensions=Dims2},
    io:format("~p~n",[relative_query_cosine(VAb,VBb)]).

%% *******************************  VECTORSPACE Query  (vectorspace is a list of Vectors)
vectorspace_add (Vector,VectorSpace) ->  [Vector|VectorSpace].

vectorspace_query(Query,Vectorspace,Threshold) ->
    [ {Vector,relative_query_cosine(Query,Vector)}  || Vector <- Vectorspace , relative_query_cosine(Query,Vector)>Threshold].

test_vectorspace_query() ->
    VA = #vector{vectorid= "somevector" },
    VB = #vector{vectorid= "somevector2" },
    Dims = [vector_dimension("dog",4),vector_dimension("cat",3)],
    VAb =VA#vector{dimensions=Dims},
    Dims2 = [vector_dimension("dog",4),vector_dimension("cat",6)],
    VBb =VB#vector{dimensions=Dims2},
    Vectorspace = vectorspace_add(VAb,[]),
    VectorspaceB = vectorspace_add(VBb,Vectorspace),

    Q = #vector{vectorid= "somevector3" },
    QDims = [vector_dimension("dog",4),vector_dimension("cat",3)],
    Qb =Q#vector{dimensions=QDims},

    vectorspace_query(Qb,VectorspaceB,0.99).

%% ***************************** BUILD IDF map for a vectorspace

%frequency of a term within a given vector
term_frequency([Dim|Dimensions],Lookfor,Total)-> term_frequency(Dimensions,Lookfor,Total+Dim#dimension.value) ;
term_frequency([],Lookfor,Total)->  Lookfor#dimension.value / Total. 
term_frequency(Dimensions,Lookfor)-> term_frequency(Dimensions,Lookfor,0).

%# of documents containing the term
term_document_frequency([Vector|Vectorspace],Lookfor,Total)->
    Dim = lists:keysearch(Lookfor#dimension.dimensionid,2,Vector#vector.dimensions),
    case Dim of
        {_, _Tuple} -> term_document_frequency(Vectorspace,Lookfor,Total+1);
        false-> term_document_frequency(Vectorspace,Lookfor,Total)
    end;
term_document_frequency([],_Lookfor,Total)->Total.
term_document_frequency(Vectorspace,Lookfor)->term_document_frequency(Vectorspace,Lookfor,0).


build_adjusted_dimension(Vectorspace,Dimension)->
    DocumentCount = length(Vectorspace),
    TDC = term_document_frequency(Vectorspace,Dimension),
    IDF = DocumentCount / TDC, 
    Dimension#dimension{ value = Dimension#dimension.value * IDF } .

build_dimension_vector_frequency(Vectorspace,Vector,Dim)->
    TF = term_frequency(Vector#vector.dimensions,Dim),
    DimTF = Dim#dimension{value=Dim#dimension.value * TF},
    build_adjusted_dimension(Vectorspace,DimTF).

create_idf_vector(Vectorspace,Vector)->
    Dimensions =  [ build_dimension_vector_frequency(Vectorspace,Vector,Dim) || Dim <- Vector#vector.dimensions ],
    Vector#vector{dimensions=Dimensions}.

create_idf_map(Vectorspace) ->
    [ create_idf_vector(Vectorspace,Vector) || Vector <- Vectorspace].


filter_dimension([Dimension|Dimensions],Threshold,Output)->
      case (Dimension#dimension.value>=Threshold) of
          true-> filter_dimension(Dimensions,Threshold,[Dimension|Output]);
          false-> filter_dimension(Dimensions,Threshold,Output)
      end;
filter_dimension([],_Threshold,Output)->Output.

%get rid of low value dimensions.
filter_vector([Vector|Vectorspace],Threshold,Output)->
    Filtered = Vector#vector{dimensions=filter_dimension(Vector#vector.dimensions, Threshold,[])},
    case length(Filtered#vector.dimensions) of
       0 -> filter_vector(Vectorspace,Threshold,Output) ;
       _ -> filter_vector(Vectorspace,Threshold,[Filtered|Output])
    end;
filter_vector([],_Threshold,Output)-> Output.

filter_vectorspace(Vectorspace,Threshold) ->
     filter_vector(Vectorspace,Threshold,[])  .


append_dimensions(DimensionsA,[],_Results)-> DimensionsA;
append_dimensions([DimA|DimensionsA],DimensionsB,Results) ->
    case lists:keysearch(DimA#dimension.dimensionid,2,DimensionsB) of
        {_,DimB} ->
            append_dimensions(DimensionsA,lists:delete(DimB,DimensionsB),[DimB#dimension{value=DimB#dimension.value+DimA#dimension.value}|Results]);
        false->
            append_dimensions(DimensionsA,DimensionsB,[DimA|Results])
    end;
append_dimensions([],DimensionsB,Results)-> lists:merge([Results,DimensionsB]).


reduce_vectorspace([Vector|Vectorspace],ResultVector)->reduce_vectorspace(Vectorspace,append_dimensions(Vector#vector.dimensions,ResultVector,[]));
reduce_vectorspace([],ResultVector)-> #vector{dimensions=ResultVector}.
reduce_vectorspace(Vectorspace)-> reduce_vectorspace(Vectorspace,[]).
   

test_idf_map()->
    VA = #vector{vectorid= "somevector" },
    VB = #vector{vectorid= "somevector2" },
    Dims = [vector_dimension("dog",4),vector_dimension("cat",3),vector_dimension("outlier",3)],
    VAb =VA#vector{dimensions=Dims},
    Dims2 = [vector_dimension("dog",4),vector_dimension("cat",6)],
    VBb =VB#vector{dimensions=Dims2},
    Vectorspace = vectorspace_add(VAb,[]),
    VectorspaceB = vectorspace_add(VBb,Vectorspace),
    IDFmap = create_idf_map(VectorspaceB),
    IDFmap.

test_reduce()->
    VA = #vector{vectorid= "somevector" },
    VB = #vector{vectorid= "somevector2" },
    VC = #vector{vectorid= "somevector3" },
    Dims = [vector_dimension("dog",4),vector_dimension("cat",3),vector_dimension("outlierA",3)],
    VAb =VA#vector{dimensions=Dims},
    Dims2 = [vector_dimension("dog",4),vector_dimension("cat",6),vector_dimension("outlierB",3)],
    VBb =VB#vector{dimensions=Dims2},
    Dims3 = [vector_dimension("dog",4),vector_dimension("cat",6),vector_dimension("outlierC",3)],
    VCb =VC#vector{dimensions=Dims3},
    Vectorspace = vectorspace_add(VAb,[]),
    VectorspaceB = vectorspace_add(VBb,Vectorspace),
    VectorspaceC = vectorspace_add(VCb,VectorspaceB),
    IDFmap = create_idf_map(VectorspaceC), 
    reduce_vectorspace(IDFmap).
    % A: dog = 4   tf= 4 / 10 = 0.4   idf= 3/3 = 1 0.4 * 4 = 1.6
    % B: dog = 4   tf= 4 / 13 = 0.308 idf = 3/3 = 1 0.308 * 4 = 1.23
    % C: 1.23
 


