#include "base.h"
#include "bigint.h"

BigInt::BigInt() : wordCount(1),negative(false),u64(0) {}
BigInt::BigInt(uint64 v) : wordCount(1) {
	*this = v;
}
BigInt::BigInt(int64 v) : wordCount(1) {
	*this = v;
}
BigInt& BigInt::operator =(const uint64 v){
	negative = false;
	wordCount = 1;
	u64 = v;
	return *this;
}
BigInt& BigInt::operator =(const int64 v){
	wordCount = 1;
	if(v<0){
		negative = true;
		u64 = (uint64)(-v);
	}else{
		negative = false;
		u64 = (uint64)(v);
	}
	return *this;
}
BigInt& BigInt::operator =(const int v){
	*this = (int64)v;
	return *this;
}
bool BigInt::operator ==(const BigInt& other) const {
	if(negative != other.negative) return false;
	if(wordCount != other.wordCount) return false;
	return u64 == other.u64;
}
bool BigInt::operator <(const BigInt& other) const {
	if(negative && (!other.negative)) return true;
	else if(other.negative && (!negative)) return false;
	else if(wordCount < other.wordCount) return true;
	
	if(negative) return u64 > other.u64;
	else return u64 < other.u64;
}

std::ostream& operator<< (std::ostream& stream,const BigInt& integer){
	if(integer.isNegative()) stream<<'-';
	stream<<integer.u64;
	return stream;
}

unittest(bigint){
	BigInt i = (uint64)22;
	assert(i.isPositive());
	assert(!i.isNegative());

	assert(i == BigInt((int64)22));
	assert(i < BigInt((uint64)88));
	assert(i <= BigInt((uint64)22));
}

