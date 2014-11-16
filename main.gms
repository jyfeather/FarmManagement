* Farm Management Main Program
SETS
         I       grain           /soybean, corn, wheat/
         J       livestock       /cow, hen/
         K       season          /WS, SF/
         S       scenario        /good, dry, flood, frost, drfr, flfr/;
PARAMETERS
         INCOME_LIVESTOCK(J)     income from livestock
                                 / cow           850
                                   hen           4.25 /
         INCOME_NEIGHBOR(K)      income from working for neighbors per hours
                                 / WS            5
                                   SF            5.5  /
         LABOR_LIMIT(K)          labor limits in both seasons
                                 / WS            4000
                                   SF            4500 /
         LIVESTOCK_NUM(J)        number of livestock they already have
                                 / cow           30
                                   hen           2000 /
         LIVESTOCK_VAL(J)        value of livestock they already have
                                 / cow           35000
                                   hen           5000 /
         LIVESTOCK_COST(J)       cost of each livstock
                                 / cow           1500
                                   hen           3 /
         LIVESTOCK_DEP(J)        depreciation of livestock
                                 / cow           0.9
                                   hen           0.75 /
         LIVESTOCK_LABOR(J)      labor required of each livestock per month
                                 / cow           10
                                   hen           0.05 /
         LIVESTOCK_ACRE(J)       land required of each livestock
                                 / cow           2
                                   hen           0 /
         LIVESTOCK_LIMIT(J)      limit of number of livestock
                                 / cow           42
                                   hen           5000 /
         FREQUENCE(S)            Scenario Frequency
                                 / good          0.4
                                   dry           0.2
                                   flood         0.1
                                   frost         0.15
                                   drfr          0.1
                                   flfr          0.05 / ;
TABLE    HOURS_GRAIN(K,I)        hours required for each grain in both seasons
                                         soybean  corn  wheat
                                 WS      1.0      0.9   0.6
                                 SF      1.4      1.2   0.7      ;
TABLE    LIVESTOCK_GRAIN(J,I)    number of grain acre to feed each livestock
                                         soybean  corn  wheat
                                 cow     0        1     0
                                 hen     0        0     0.05     ;
TABLE    NET_INCOME(S,I)          net income in different scenario
                                         soybean  corn  wheat
                                 good    70       60    40
                                 dry     -10      -15   0
                                 flood   15       20    10
                                 frost   50       40    30
                                 drfr    -15      -20   -10
                                 flfr    10       10    5        ;
SCALAR
         FARM_AREA       total farm area                 /640/
         INVESTMENT      total money for investment      /20000/
         EXPENSES        annual expenses                 /40000/;

* Define Model
VARIABLE                 TOT     income  ;
POSITIVE VARIABLES
                         X(I)    acre of each grain
                         Z(K)    hours working for neighbor ;
INTEGER VARIABLE         Y(J)    number of new purchased livestock;
EQUATIONS
         INC_IND         income for individual scenarios
         INC_WHL         income for whole scenarios
         ACRE            land limit
         HOURS(K)        hours limit in each season
         INVEST          money limit
         CAP(J)          capacity of livestock
         FEED(I)         each grain required of livestock;
INC_IND  ..      TOT =E= SUM(J, (LIVESTOCK_NUM(J)+Y(J))*INCOME_LIVESTOCK(J)) +
                         SUM(I, NET_INCOME('good',I)*X(I)) +
                         INVESTMENT-SUM(J, LIVESTOCK_COST(J)*Y(J)) +
                         SUM(J, (LIVESTOCK_VAL(J)+LIVESTOCK_COST(J)*Y(J))*
                                 LIVESTOCK_DEP(J)) +
                         SUM(K, INCOME_NEIGHBOR(K)*Z(K)) - EXPENSES;
INC_WHL  ..      TOT =E= SUM(J, (LIVESTOCK_NUM(J)+Y(J))*INCOME_LIVESTOCK(J)) +
                         SUM(I, SUM(S, NET_INCOME(S,I)*FREQUENCE(S))*X(I)) +
                         INVESTMENT-SUM(J, LIVESTOCK_COST(J)*Y(J)) +
                         SUM(J, (LIVESTOCK_VAL(J)+LIVESTOCK_COST(J)*Y(J))*
                                 LIVESTOCK_DEP(J)) +
                         SUM(K, INCOME_NEIGHBOR(K)*Z(K)) - EXPENSES;
ACRE     ..      SUM(J, LIVESTOCK_ACRE(J)*(LIVESTOCK_NUM(J)+Y(J))) +
                         SUM(I, X(I)) =L= FARM_AREA;
HOURS(K) ..      SUM(J, LIVESTOCK_LABOR(J)*(LIVESTOCK_NUM(J)+Y(J))*6)+
                         SUM(I, HOURS_GRAIN(K,I)*X(I))+Z(K) =E= LABOR_LIMIT(K);
INVEST   ..      SUM(J, LIVESTOCK_COST(J)*Y(J)) =L= INVESTMENT;
CAP(J)   ..      LIVESTOCK_NUM(J) + Y(J) =L= LIVESTOCK_LIMIT(J);
FEED(I)  ..      X(I) =G= SUM(J, LIVESTOCK_GRAIN(J,I)*(LIVESTOCK_NUM(J)+Y(J)));

* Solve the model
*MODEL    origin      /INC_IND, ACRE, HOURS, INVEST, CAP, FEED/;
MODEL    origin      /INC_WHL, ACRE, HOURS, INVEST, CAP, FEED/;

* Sensitivity Analysis
origin.optfile = 1;
$onecho > cplex.opt
lpmethod 2
objrng all
rhsrng all
$offecho

SOLVE    origin      USING MIP MAXIMIZING TOT;




