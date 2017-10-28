let data = [13; -3; -25; 20; -3; -16; -23; 18; 20; -7; 12; -5; -22; 15; -4; 7]

let leftFold item (leftSum, sum, maxLeft, i) = 
    let newSum = sum + item
    if newSum > leftSum 
    then (newSum, newSum, i, i)
    else (leftSum, newSum, maxLeft, i - 1)

let maxCrossingSubarrayLeft (arr: int list) low mid =
    let result = 
        List.foldBack leftFold arr.[low .. mid] (-999, 0, 0, mid - low)
    let leftSum, _, maxLeft, _ = result
    (maxLeft + low, leftSum)

//------------------------------------------------------

let rightFold (rightSum, sum, maxLeft, i) item = 
    let newSum = sum + item
    if newSum > rightSum 
    then (newSum, newSum, i, i)
    else (rightSum, newSum, maxLeft, i + 1)

let maxCrossingSubarrayRight (arr: int list) mid high =
    let result = 
        arr.[mid .. high] 
        |> List.fold rightFold (-999, 0, 0, 0)
    let leftSum, sum, maxRight, i = result
    (maxRight + mid + 1, leftSum)    


//------------------------------------------------------


let findMaxCrossingSubarray arr low mid high = 
    let maxLeft, leftSum = maxCrossingSubarrayLeft arr low mid
    let maxRight, rightSum = maxCrossingSubarrayRight arr (mid + 1) high
    (maxLeft, maxRight, leftSum + rightSum)

//------------------------------------------------------

let findMaximumSubaray arr =
    let rec findMaxSub (arr: int list)  low high = 
        if high = low then  
            (low, high, arr.[low])
        else
            let mid = (low + high) / 2
            let leftLow, leftHigh, leftSum = findMaxSub arr low mid
            let rightLow, rightHigh, rightSum = findMaxSub arr (mid + 1) high
            let crossLow, crossHigh, crossSum = findMaxCrossingSubarray arr low mid high
            if leftSum >= rightSum && leftSum >= crossSum then
                (leftLow, leftHigh, leftSum)
            elif rightSum >= leftSum && rightSum >= crossSum then
                (rightLow, rightHigh, rightSum)
            else
                (crossLow, crossHigh, crossSum)
    findMaxSub arr 0 (arr.Length - 1)

findMaximumSubaray data

