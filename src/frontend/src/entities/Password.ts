import { isStrongPassword } from 'validator'

export function getIsPasswordStrong(value: string, returnScore = false): boolean | number {
  return isStrongPassword(value, {
    // TODO configure sensitivity
    minLength: 8,
    minLowercase: 1,
    minUppercase: 1,
    minNumbers: 1,
    minSymbols: 1,
    returnScore: returnScore as any,
    pointsPerUnique: 1,
    pointsPerRepeat: 0.5,
    pointsForContainingLower: 20,
    pointsForContainingUpper: 20,
    pointsForContainingNumber: 20,
    pointsForContainingSymbol: 20,
  })
}
