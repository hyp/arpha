/**
* This module provides a big integer strcture
*/
#ifndef ARPHA_BIGINT_H
#define ARPHA_BIGINT_H

struct BigInt {
//TODO protected:
	union {
		uint64 u64;
		void*  data;
	};
	int wordCount;
	bool negative;
	
public:
	BigInt();
	BigInt(uint64 v);
	BigInt(int64 v);
	BigInt& operator =(const uint64 v);
	BigInt& operator =(const int64 v);
	BigInt& operator =(const int v);

	bool operator ==(const BigInt& other) const;
	bool operator <(const BigInt& other) const;
	inline bool operator <=(const BigInt& other) const { return (*this < other) || (*this == other); }

	inline bool isNegative() const { return negative; }
	inline bool isPositive() const { return !negative; }
	inline bool isZero() const { return wordCount == 1 && u64 == 0; }

	void changeSign(){ negative = !negative; }

	friend std::ostream& operator<< (std::ostream& stream,const BigInt& integer);
}; 

std::ostream& operator<< (std::ostream& stream,const BigInt& integer);

#endif