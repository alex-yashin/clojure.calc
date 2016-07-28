(ns app.core)
(require '[clojure.string :as str])

(let [zero (int \0)
      nine (int \9)]
  (defn isDigit? [c] (<= zero (int c) nine)))

(defn isOperator? [c]
    (cond 
        (= c "+") true
        (= c "-") true
        (= c "*") true
        (= c "/") true
        :else false))


(defn getOperatorPriority [c]
    (cond 
        (= c "+") 1
        (= c "-") 1
        (= c "*") 2
        (= c "/") 2
        :else 0))

(defn parseToken [agg elem]
    (let [[arr s] agg]
        (cond 
            (= elem \ ) [arr s]
            (isDigit? elem) [arr (str s elem)]
            (= s "") [(conj arr (str "" elem)) ""]
            :else [(conj (conj arr s) (str "" elem)) ""])))

(defn parseTokens [condition]
    (let [[arr s] (reduce parseToken [[] ""] condition)]
        (cond 
            (= s "") arr
            :else (conj arr s))))

(defn makePostfixIterationDebug [agg elem]
    (println elem))

(defn moveOperatorsFromStack [agg]
    (let [[out stack] agg top (last stack)]
        (cond 
            (not (isOperator? top)) [out stack]
            :else (moveOperatorsFromStack [(conj out top) (pop stack)]))))

(defn addToStack[agg elem]
    (let [[out stack] agg]
        [out (conj stack elem)]))

(defn removeParenthesFromStack[agg]
    (let [[out stack] agg top (last stack)]
        (cond 
            (= top "(") [out (pop stack)]
            :else [out stack])))

(defn makePostfixIteration [agg elem]
    (let [[out stack] agg]
        (cond 
            (isDigit? (first elem)) [(conj out elem) stack]
            (isOperator? elem) 
                (let [top (last stack)]
                    (cond
                        (isOperator? top)
                            (cond
                                (> (getOperatorPriority elem) (getOperatorPriority top)) [out (conj stack elem)]
                                :else (addToStack (moveOperatorsFromStack [out stack]) elem))
                        :elem [out (conj stack elem)]))
            (= elem "(") [out (conj stack elem)]
            (= elem ")") (removeParenthesFromStack (moveOperatorsFromStack [out stack]))
            :else [out stack])))

(defn makePostfix [tokens]
    (let [[postfix _] (moveOperatorsFromStack (reduce makePostfixIteration [[] []] tokens))]
        postfix))

(defn calculateIteration [stack elem]
    (cond 
        (isDigit? (first elem)) (conj stack elem)
        (isOperator? elem)
            (let [secondArgument (last stack)]
                (do
                    (let [firstArgument (last (pop stack))]
                            (cond 
                                (= elem "+") (conj (pop (pop stack)) (+ (bigint firstArgument) (bigint secondArgument)))
                                (= elem "-") (conj (pop (pop stack)) (- (bigint firstArgument) (bigint secondArgument)))
                                (= elem "*") (conj (pop (pop stack)) (* (bigint firstArgument) (bigint secondArgument)))
                                (= elem "/") (conj (pop (pop stack)) (/ (bigint firstArgument) (bigint secondArgument)))))))))

(defn calculatePostfix [postfix]
    (first (reduce calculateIteration [] postfix)))

(defn calculate [equation]
    (calculatePostfix (makePostfix (parseTokens equation))))
    
(defn -main [& args]
    (do 
        (println (calculate "12+2-4*(4-1)"))))