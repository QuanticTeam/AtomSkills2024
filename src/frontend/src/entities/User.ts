import { TFunction, t } from 'i18next'
import { z } from 'zod'
import { apiClient } from '~/shared/apiClient'

export const UserApi = {
  async signUp(dto: UserSignUpDto) {
    return await apiClient.post('/User/SignUp', dto)
  },
  async signIn(dto: UserSignInDto) {
    return (await apiClient.post('/User/SignIn', dto)).data
  },
}

// Extracted from UserApi, because TS goes mad from circular ref dto type
async function checkLogin(login: string) {
  return !(await apiClient.post('/User/CheckLogin', { login })).data.taken
}

export const getUserSignUpSchema = (t: TFunction) =>
  z.object({
    login: z
      .string()
      .trim()
      .min(1, t('fieldLoginErrRequired'))
      .min(2, t('fieldLoginErrShort'))
      .max(56, t('fieldLoginErrLong'))
      .regex(/^[A-Za-z0-9]+$/, t('fieldLoginErrSym'))
      .refine(checkLogin, t('fieldLoginErrTaken')),
    // fullname: z.string().trim().min(1, t('fieldFullnmeErrRequired')),
    password: z
      .string()
      .trim()
      .min(1, t('fieldPasswordErrRequired'))
      .min(8, t('fieldPasswordErrShort'))
      .max(32, t('fieldPasswordErrLong'))
      .regex(/^[A-Za-z0-9-_+!?=#$%&@^`~]+$/, t('fieldPasswordErrSym')),
    // .refine(p => !getIsPasswordStrong(p), t('Password is too weak'))), // TODO
    role: z.number(),
  })

export type UserSignUpDto = z.infer<ReturnType<typeof getUserSignUpSchema>>

export const getUserSignInSchema = (t: TFunction) =>
  z.object({
    login: z.string().trim().min(1, t('fieldLoginErrRequired')),
    password: z.string().trim().min(1, t('fieldPasswordErrRequired')),
    remember: z.boolean(),
  })

export type UserSignInDto = z.infer<ReturnType<typeof getUserSignInSchema>>
